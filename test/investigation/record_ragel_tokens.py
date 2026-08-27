#!/usr/bin/env python3
"""Record frozen Devkit/Ragel token streams for corpus and regression literals."""

from pathlib import Path
import shutil
import subprocess
import sys
import tempfile


def ocaml_strings(path: Path):
    text = path.read_text(encoding="utf-8")
    values = []
    i = 0
    while i < len(text):
        if text[i] != '"':
            i += 1
            continue
        i += 1
        value = bytearray()
        while i < len(text):
            c = text[i]
            i += 1
            if c == '"':
                values.append(bytes(value))
                break
            if c != "\\":
                value.extend(c.encode("utf-8"))
                continue
            if i >= len(text):
                raise ValueError(f"unterminated escape in {path}")
            c = text[i]
            i += 1
            simple = {"n": 10, "r": 13, "t": 9, "b": 8, "\\": 92, '"': 34, "'": 39, " ": 32}
            if c in simple:
                value.append(simple[c])
            elif c.isdigit():
                digits = c + text[i : i + 2]
                if len(digits) != 3 or not digits.isdigit():
                    raise ValueError(f"bad decimal escape in {path}")
                value.append(int(digits, 10))
                i += 2
            elif c == "x":
                digits = text[i : i + 2]
                value.append(int(digits, 16))
                i += 2
            elif c == "o":
                digits = text[i : i + 3]
                value.append(int(digits, 8))
                i += 3
            elif c == "u" and i < len(text) and text[i] == "{":
                end = text.index("}", i + 1)
                value.extend(chr(int(text[i + 1 : end], 16)).encode("utf-8"))
                i = end + 1
            elif c == "\n":
                while i < len(text) and text[i] in " \t":
                    i += 1
            else:
                value.extend(c.encode("utf-8"))
        else:
            raise ValueError(f"unterminated string in {path}")
    return values


def main():
    root = Path(__file__).resolve().parents[2]
    corpus = Path(sys.argv[1]) if len(sys.argv) > 1 else root / "big_tests"
    output = Path(sys.argv[2]) if len(sys.argv) > 2 else root / "scratch/common-ragel-tokens.bin"
    output.parent.mkdir(parents=True, exist_ok=True)

    with tempfile.TemporaryDirectory(prefix="ragel-regressions-", dir=output.parent) as temporary:
        regression_dir = Path(temporary)
        count = 0
        for source in sorted((root / "test" / "lite").glob("*regression.ml")):
            for index, value in enumerate(ocaml_strings(source), 1):
                name = f"{source.stem}-{index:04d}.input"
                (regression_dir / name).write_bytes(value)
                count += 1

        subprocess.run(
            [
                "dune",
                "exec",
                "--profile",
                "release",
                "test/investigation/record_ragel_tokens.exe",
                "--",
                str(output),
                str(corpus),
                str(regression_dir),
            ],
            cwd=root,
            check=True,
        )

    compressed = output.with_suffix(output.suffix + ".zst")
    subprocess.run(["zstd", "-T0", "-f", "--rm", str(output), "-o", str(compressed)], check=True)
    print(f"recorded {count} regression literals in {compressed}")


if __name__ == "__main__":
    main()

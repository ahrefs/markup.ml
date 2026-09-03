/* This file is part of Markup.ml, released under the MIT license. See
   LICENSE.md for details, or visit https://github.com/aantron/markup.ml. */

#include <caml/mlvalues.h>
#include <string.h>

CAMLprim value markup_lite_string_helpers_find(
    value string, value start, value character)
{
  const char *data = String_val(string);
  const mlsize_t length = caml_string_length(string);
  const mlsize_t offset = Long_val(start);
  const char *found =
      memchr(data + offset, Int_val(character), length - offset);
  return Val_long(found == NULL ? length : (mlsize_t)(found - data));
}

CAMLprim value markup_lite_string_helpers_count(
    value string, value start, value stop, value character)
{
  const unsigned char *data = (const unsigned char *)String_val(string);
  const mlsize_t first = Long_val(start);
  const mlsize_t last = Long_val(stop);
  const unsigned char byte = Int_val(character);
  mlsize_t count = 0;

  for (mlsize_t index = first; index < last; ++index)
    count += data[index] == byte;

  return Val_long(count);
}

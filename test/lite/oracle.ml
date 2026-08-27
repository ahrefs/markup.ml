let parse ?depth_limit
    ?(context : [ `Document | `Fragment of string ] = `Document) report html =
  Markup.string html
  |> Markup.parse_html ~report ~context ?depth_limit
  |> Markup.signals

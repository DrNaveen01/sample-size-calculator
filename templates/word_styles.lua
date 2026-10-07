-- The report title uses Word's Title style; remaining headings keep their hierarchy.
local original_proportion_report = false
function Header(element)
  if element.level == 1 then
    local title = pandoc.utils.stringify(element.content)
    original_proportion_report = title == "Sample size calculation for a single population proportion"
    return pandoc.Div({pandoc.Para(element.content)}, pandoc.Attr("", {}, {["custom-style"] = "Title"}))
  end
  local title = pandoc.utils.stringify(element.content)
  -- Comparison reports flow naturally so an extra formula cannot leave a nearly empty page.
  if element.level == 2 and original_proportion_report and (title == "Numerical substitution" or title == "Interpretation") then
    return pandoc.Div({pandoc.Para(element.content)}, pandoc.Attr("", {}, {["custom-style"] = "Report Section Start"}))
  end
  element.level = element.level - 1
  return element
end

-- Formula source length should not widen the short symbol column.
function Table(element)
  if not original_proportion_report and #element.colspecs == 3 then
    element.colspecs = {
      {pandoc.AlignLeft, 0.16},
      {pandoc.AlignLeft, 0.56},
      {pandoc.AlignRight, 0.28}
    }
  end
  return element
end

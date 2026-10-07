-- The report title uses Word's Title style; remaining headings keep their hierarchy.
function Header(element)
  if element.level == 1 then
    return pandoc.Div({pandoc.Para(element.content)}, pandoc.Attr("", {}, {["custom-style"] = "Title"}))
  end
  local title = pandoc.utils.stringify(element.content)
  if element.level == 2 and (title == "Numerical substitution" or title == "Interpretation") then
    return pandoc.Div({pandoc.Para(element.content)}, pandoc.Attr("", {}, {["custom-style"] = "Report Section Start"}))
  end
  element.level = element.level - 1
  return element
end

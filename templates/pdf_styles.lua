-- Keep the legend heading together with its table header and first rows.
function Header(element)
  if element.level == 2 and pandoc.utils.stringify(element.content) == "Legends and input values" then
    return {pandoc.RawBlock("latex", "\\ReserveLegendSpace"), element}
  end
end

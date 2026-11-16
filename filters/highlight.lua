-- Pandoc has no highlighter for Happy or Alex source. Yacc and Lex are close.
local highlighters = { happy = "yacc", alex = "lex" }

function CodeBlock(block)
  for i, class in ipairs(block.classes) do
    if highlighters[class] then
      block.classes[i] = highlighters[class]
    end
  end
  return block
end

-- Removes the literate programming metadata that Pandoc copies from Org.
-- The reader of a post has no use for tangle targets and block names.

local block_args = { "tangle", "exports", "noweb" }

function CodeBlock(block)
  for _, key in ipairs(block_args) do
    block.attributes[key] = nil
  end
  -- A code block gets an identifier only from '#+name:'.
  block.identifier = ""
  return block
end

function Header(header)
  for key, _ in pairs(header.attributes) do
    if key:match("^header%-args") then
      header.attributes[key] = nil
    end
  end
  return header
end

-- Pandoc passes '#+PROPERTY:' through as raw Org.
function RawBlock(raw)
  if raw.format == "org" and raw.text:match("^#%+PROPERTY:") then
    return {}
  end
end

defmodule FORMS do
  require Record

  Enum.each(Record.extract_all(from_lib: "forms/include/meta.hrl"), fn {name,
                                                                        definition} ->
    Record.defrecord(name, definition)
  end)

  defmacro __using__(opts \\ []) do
    imports =
      opts
      |> Macro.expand(__CALLER__)
      |> Keyword.get(:with, [:n2o])

    Enum.map(imports, fn mod ->
      if Code.ensure_compiled?(mod) do
        upcased = Module.concat([String.upcase(to_string(mod))])

        quote do
          import unquote(upcased)
          alias unquote(mod), as: unquote(upcased)
        end
      else
        IO.warn("🚨 Unknown module #{mod} was requested to be used by ::forms. Skipping.")
      end
    end)
  end
end

defmodule FORM.Mixfile do
  use Mix.Project
  def application, do: [mod: {:form, []}]

  def project do
    [
      app: :form,
      version: "4.7.0",
      description: "FORM Business X-Forms",
      package: package(),
      deps: deps()
    ]
  end

  def package do
    [
      files: ~w(doc include man lib src mix.exs rebar.config LICENSE),
      licenses: ["ISC"],
      maintainers: ["Namdak Tonpa"],
      links: %{"GitHub" => "https://github.com/synrc/forms"}
    ]
  end

  def deps do
    [
      {:ex_doc, "~> 0.11", only: :dev},
      {:nitro, "~> 4.7.0"}
    ]
  end
end

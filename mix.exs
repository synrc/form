defmodule FORMS.Mixfile do
  use Mix.Project
  def application, do: [mod: {:forms, []}]

  def project do
    [
      app: :forms,
      version: "4.6.0",
      description: "FORMS",
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
      {:nitro, "~> 4.4.1"}
    ]
  end
end

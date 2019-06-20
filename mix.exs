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
      links: %{"GitHub" => "https://github.com/synrc/forms"}
    ]
  end

  def deps do
    [
      {:nitro, github: "synrc/nitro"},
      {:n2o, github: "synrc/n2o"}
    ]
  end
end

defmodule FORM.Mixfile do
  use Mix.Project
  def application, do: [mod: {:form, []}]

  def project do
    [
      app: :form,
      version: "7.1.0",
      description: "FORM Business X-Forms",
      package: package(),
      deps: deps()
    ]
  end

  def package do
    [
      files: ~w(doc include man lib priv src mix.exs rebar.config LICENSE),
      licenses: ["ISC"],
      maintainers: ["Namdak Tonpa"],
      links: %{"GitHub" => "https://github.com/synrc/form"}
    ]
  end

  def deps do
    [
      {:ex_doc, "~> 0.25.3", only: :dev},
      {:nitro, "~> 6.10.8"}
    ]
  end
end

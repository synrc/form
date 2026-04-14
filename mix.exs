defmodule FORM.Mixfile do
  use Mix.Project
  def application, do: [mod: {:form, []}]

  def project do
    [
      app: :form,
      version: "11.4.14",
      description: "ERP/1 X-FORM Business Cartulary",
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
      {:ex_doc, ">= 0.0.0", only: :dev},
      {:nitro, "~> 11.4.14"}
    ]
  end
end

defmodule FORM.Mixfile do
  use Mix.Project
  def application, do: [mod: {:form, []}]

  def project do
    [
      app: :form,
      version: "11.4.15",
      description: "ERP/1 X-FORM Business Cartulary",
      package: package(),
      deps: deps(),
      erlc_options: [:nowarn_export_all, :nowarn_deprecated_function]
    ]
  end

  def package do
    [
      files: ~w(include man lib priv src mix.exs LICENSE README.md),
      licenses: ["ISC"],
      maintainers: ["Namdak Tonpa"],
      links: %{"GitHub" => "https://github.com/synrc/form"}
    ]
  end

  def deps do
    [
      {:ex_doc, ">= 0.0.0", only: :dev, runtime: false},
      {:nitro, "~> 11.4.14"}
    ]
  end
end

FORM: Declarative Documents
============================

[![Build Status](https://travis-ci.org/synrc/form.svg?branch=master)](https://travis-ci.org/synrc/form)
[![Hex pm](http://img.shields.io/hexpm/v/form.svg?style=flat)](https://hex.pm/packages/form)

Intro
-----

```
  #document { name = Name, sections = [
      #sec { name=[<<"Input the password "
                     "you have received by SMS"/utf8>>,
             nitro:to_list(Phone#phone.number)] } ],

    buttons  = [ #but { name='decline',
                        title=<<"Cancel"/utf8>>,
                        class=cancel,
                        postback={'CloseOpenedForm',Name} },

                 #but { name='proceed',
                        title = <<"Proceed"/utf8>>,
                        class = [button,sgreen],
                        sources = [otp],
                        postback = {'Spinner',{'OpenForm',Name}}}],

    fields = [ #field { name='otp',
                        type=otp,
                        title= <<"Password:"/utf8>>,
                        labelClass=label,
                        fieldClass=column3}]}.

```

Spec
----

Documents or Forms consist of set of fields grouped
in sections and a row of control buttons.
It mey also contain fields of customizable types.

```
  -record(document, { ?ITERATOR(feed),
                      name,
                      base,
                      sections,
                      fields,
                      buttons,
                      access }).
```

```
 -record(field,     { id, sec=1, name, pos, title,
                      layout, visible=true,
                      disabled=false, format="~w",
                      curr=[], postfun=[], desc,
                      wide=normal, type=binary,
                      etc, labelClass=label,
                      fieldClass=field,
                      boxClass=box,
                      access, tooltips=[],
                      options=[], min=0, max=1000000,
                      length=10, postback }).
```

```
 -record(sec,       { id,
                      name,
                      desc="" }).
```
```
 -record(but,       { id,
                      postback,
                      name,
                      title,
                      sources=[],
                      class }).
```
```
  -record(sel,      { id,
                      postback,
                      name,
                      title }).
```
```
 -record(opt,       { id,
                      postback,
                      name,
                      title,
                      checked=false,
                      disabled=false,
                      noRadioButton=false }).
```
```
 -record(validation, { name, type, msg,
                       extract = fun(X) -> X end,
                       options=[], function,
                       field={record,pos} }).
```

KVX Data Model
--------------

The Metainformation is used to generate KVS Data Model.
The [KVS](http://github.com/synrc/kvx) layer provides persistence.

NITRO Applications
-----------------

JavaScript Web Application is generated using Metainformation and Data Model.
[N2O](http://github.com/synrc/n2o) is used as a domain specific language to generate forms.
JavaScript/OTP is used to generate form. Average rendering speed is 25 forms per second.
Erlang and JavaScript/OTP are used to define validation
rules applied to documents during workflow.

BPE Processes
-------------

Workflows are complimentary to business rules and could be specified separately.
[BPE](http://github.com/synrc/bpe) defenitions provide front API to end-user applications.

Credits
-------

* Maxim Sokhatsky

OM A HUM

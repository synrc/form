FORMS: Erlang Business Forms
============================

[![Build Status](https://travis-ci.org/synrc/forms.svg?branch=master)](https://travis-ci.org/synrc/forms)

Metainformation
---------------

This project is a declarative framework for documents and forms.
[Metainformation](include/meta.hrl) is a declaration of Documents and their fields.

Data Model
----------

The Metainformation is used to generate KVS Data Model. 
The [KVS](http://github.com/synrc/kvs) layer along with FEEDS server provide persistence.

Application
-----------

JavaScript Web Application is generated using Metainformation and Data Model.
[N2O](http://github.com/5HT/n2o) is used as a domain specific language to generate forms.
JavaScript/OTP is used to generate forms. Average rendering speed is 25 forms per second.

![Forms](http://synrc.com/lj/Forms.png)


Validation Rules
----------------

Erlang and JavaScript/OTP are used to define validation
rules applied to documents during workflow.

Business Rules
--------------

Business rules should be specified by developers.
RETE is used for rules specifications which can be triggered during workflow.

Workflow Scenarios
------------------

Workflows are complimentary to business rules and could be specified separately.
BPE defenitions provide front API to end-user applications.

Credits
-------

* Maxim Sokhatsky

OM A HUM

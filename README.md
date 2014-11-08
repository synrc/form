Erlang Business Forms
=====================

Metainformation
---------------

Metainformation declares the Documents and its fields.
This project is a Metainformation project for core document/forms model definition.

Data Model
----------

KVS Data Model is being generated from Metainformation.
KVS layer along with FEEDS server provide persistance facilities.

Application
-----------

JavaScript Web Application is generated using Metainformation and Data Model.
N2O is used as DSL language for forms generation.
JavaScript/OTP is used for generating forms. Forms average render speed is 25 FPS forms per second.

![Forms](http://synrc.com/lj/Forms.png)


Validation Rules
----------------

Validation rules should be applied by developer.
Erlang and JavaScript/OTP is used to define validation
rules applied to documents during workflow.

Business Rules
--------------

Business rules should be specified by developers.
RETE is used for rules specifications which can be triggered during workflow.

Workflow Scenarios
------------------

Workflows are complimentary to business rules and could be specified separetly.
BPE defenitions provides front API to the end-user application.

Credits
-------

* Maxim Sokhatsky

OM A HUM

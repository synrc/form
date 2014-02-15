-module(bank).
-compile(export_all).
-include("meta.hrl").

documents() ->
    [
        registration_application(),
        registration(),
        account(),
        transfer(),
        wire_transfer(),
        card()
    ].

registration_application() ->
    #document{name=registration_application,fields=[
        #field{pos=01,name=id,          type=binary,title="ID",visible=false},
        #field{pos=02,name=givenname,   type=string,title="Given Name",wide=small},
        #field{pos=03,name=sn,          type=string,title="Surname",wide=small},
        #field{pos=04,name=cn,          type=string,title="Common Name",visible=false},
        #field{pos=05,name=taxid,       type=binary,title="Tax Number"},
        #field{pos=06,name=address,     type=binary,title="Address"},
        #field{pos=07,name=zip,         type=integer,title="ZIP",wide=small},
        #field{pos=08,name=country,     type=string,title="Country",wide=small},
        #field{pos=09,name=phone,       type=string,title="Phone",wide=small},
        #field{pos=10,name=mail,        type=integer,title="Mail",wide=small},
        #field{pos=11,name=account_type,type=string,title="Account Type"},
        #field{pos=12,name=attachments, type={feed,attachment},title="Attachments"},
        #field{pos=13,name=status,      type=string,title="Status"},
        #field{pos=14,name=date,        type=date,title="Date of Application"}
    ]}.

registration() ->
    #document{name=registration,fields=[
        #field{pos=01,name=id,          type=binary,title="ID",visible=false},
        #field{pos=02,name=registration,type=binary,title="Registration"},
        #field{pos=03,name=application, type=binary,title="Original Application"},
        #field{pos=04,name=status,      type=string,title="Status"},
        #field{pos=05,name=status,      type=string,title="Status"},
        #field{pos=06,name=date,        type=date,title="Date of Registration"}
    ]}.

account() ->
    #document{name=account,fields=[
        #field{pos=01,name=id,          type=binary,title="ID",visible=false},
        #field{pos=02,name=account,     type=binary,title="Account"},
        #field{pos=03,name=registration,type=binary,title="Registration"},
        #field{pos=04,name=currency,    type=string,title="Currency"},
        #field{pos=05,name=transactions,type={feed,transfer},title="Transactions"},
        #field{pos=06,name=cards,       type={feed,card},title="Transactions"},
        #field{pos=07,name=ballance,    type=money,title="Balance"},
        #field{pos=08,name=type,        type=string,title="Account Type"},
        #field{pos=09,name=status,      type=string,title="Status"},
        #field{pos=10,name=date,        type=date,title="Date of Account Opening"}
    ]}.

transfer() ->
    #document{name=transfer,fields=[
        #field{pos=02,name=id,          type=binary,title="ID",visible=false},
        #field{pos=03,name=transfer,    type=binary,title="Transfer"},
        #field{pos=04,name=beneficiary, type=binary,title="Beneficiary Account"},
        #field{pos=05,name=to,          type=binary,title="Beneficiary Name"},
        #field{pos=06,name=from,        type=binary,title="Subsidiary Name"},
        #field{pos=07,name=subsidiary,  type=binary,title="Subsidiary Account"},
        #field{pos=08,name=amount,      type=money,title="Amount"},
        #field{pos=09,name=currency,    type=string,title="Currency"},
        #field{pos=10,name=status,      type=string,title="Status"},
        #field{pos=11,name=date,        type=date,title="Date of Operation"}
    ]}.

wire_transfer() ->
    #document{name=swift,base=transfer,fields=[
        #field{pos=12,name=swift,       type=binary,title="Beneficiary Bank Code"}
    ]}.

card() ->
    #document{name=card,fields=[
        #field{pos=01,name=id,          type=binary,title="ID",visible=false},
        #field{pos=02,name=card,        type=binary,title="Card Number"},
        #field{pos=03,name=holder,      type=binary,title="Card Holder"},
        #field{pos=05,name=valid,       type=date,title="Valid To"},
        #field{pos=06,name=cvv2,        type=date,title="CVV2"},
        #field{pos=07,name=status,      type=date,title="Status"},
        #field{pos=04,name=date,        type=date,title="Issued Date"}
    ]}.

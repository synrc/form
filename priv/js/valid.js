var Validation = {

    "money": function(e, min, max, msg) {
        var money = e.detail.replace(/ /g,"");
        if( !/^(0\.\d{1,2}|[1-9]{1}\d{0,}(\.\d{1,2}){0,1})$/.test(money) ) {
            showErrorMSG(e.target, msg);
            return false; }
        if(!this.min(e, min)) return false;
        if(max!='none' && parseFloat(money) > max){
            showErrorMSG(e.target, i18n("MaxAmount")+max+" "+i18n(currency));
            return false; }
        var parent = e.target.parentNode;
        if(parent.lastChild.tagName == "DIV" && parent.lastChild.className == "errorMSG") parent.removeChild(parent.lastChild);
        return true;
    },
     "pay": function(e, min, max, deptype, msg) {
            var money = document.querySelector("input[id^='money']").value;
            var pay = e.detail.replace(/ /g,"");
            if( !/^(0\.\d{1,2}|[1-9]{1}\d{0,}(\.\d{1,2}){0,1})$/.test(pay) ) {
                showErrorMSG(e.target, msg);
                return false; }
            if(!this.min(e, min)) return false;
            var maxSum = (deptype == 'DPBN') ? max : money;
            if(money!=undefined && parseFloat(pay) > maxSum) {
                showErrorMSG(e.target, i18n("MaxPayAmount")+money+" "+i18n(currency));
                return false; }
            var parent = e.target.parentNode;

            if(parent.lastChild.tagName == "DIV" && parent.lastChild.className == "errorMSG") parent.removeChild(parent.lastChild);
            return true;
        },

    "date": function(e) {
        var min, max, val = /^{(\d{4}),(\d{2}),(\d{2})}$/.exec(e.detail);
        if(val == null) { showErrorMSG(e.target, i18n("WrongDate")); return false; }

        min = /^(\d{4})-(\d{2})-(\d{2})$/.exec(e.target.getAttribute("min"));
        max = /^(\d{4})-(\d{2})-(\d{2})$/.exec(e.target.getAttribute("max"));
        if(min==null || max==null) { showErrorMSG(e.target, i18n("RefreshPage")); return false; }

        val = parseInt(val[1]+val[2]+val[3]);
        min = parseInt(min[1]+min[2]+min[3]);
        max = parseInt(max[1]+max[2]+max[3]);
        if(val < min) { showErrorMSG(e.target, i18n("DateMin")); return false; }
        if(val > max) { showErrorMSG(e.target, i18n("DateMax")); return false; }
        if((e.target.parentNode).lastChild.tagName == "DIV" && (e.target.parentNode).lastChild.className == "errorMSG") (e.target.parentNode).removeChild((e.target.parentNode).lastChild);
        return true;
    },

    "nums": function(e, mim, max) {
        console.log(e);
        return true;
    },

    "comboLookup": function(e, min, max) {
        console.log(e);
        return true;
    },

    "comboLookupVec": function(e, min, max) {
        console.log(e);
        return true;
    },

    "calendar": function(e) {
        var min, max, val = (e.detail instanceof Date) ? e.detail : null;
        if(val == null || val.toString() == "Invalid Date") { showErrorMSG(e.target, i18n("WrongDate")); return false; }
        if(!val.getDate()) { showErrorMSG(e.target, i18n("DateInterval")); return false; }

        if(e.target.getAttribute("type")=="calendar") {
            var picker = pickers[e.target.id];
            if(picker) {
                if(picker._d < picker._o.minDate) { showErrorMSG(e.target, i18n("DateMin")); return false; }
                if(picker._d > picker._o.maxDate) { showErrorMSG(e.target, i18n("DateMax")); return false; }
            }else { return false; }
        }else {
            min = /^(\d{4})-(\d{2})-(\d{2})$/.exec(e.target.getAttribute("min"));
            max = /^(\d{4})-(\d{2})-(\d{2})$/.exec(e.target.getAttribute("max"));
            if(min==null || max==null) { showErrorMSG(e.target, i18n("RefreshPage")); return false; }

            min = new Date(Date.parse(e.target.getAttribute("min")));
            max = new Date(Date.parse(e.target.getAttribute("max")));
            if(val < min) { showErrorMSG(e.target, i18n("DateMin")); return false; }
            if(val > max) { showErrorMSG(e.target, i18n("DateMax")); return false; }
        }
        if((e.target.parentNode).lastChild.tagName == "DIV" && (e.target.parentNode).lastChild.className == "errorMSG") (e.target.parentNode).removeChild((e.target.parentNode).lastChild);
        return true;
    },

    "card": function(e, msg) {
        var elId = "no_card_warning";
        if (e.target.selectedIndex == 0 && e.target.length != 1) {
            showErrorMSG(e.target, msg || "", elId);
            return false;
        } else if (e.target.value == "nocard") {
            showErrorMSG(e.target, msg || "", elId);
            return false;
        } else {
            var divmsg = (e.target.parentNode).querySelector("#"+elId) || null;
            if(divmsg != null) (divmsg.parentNode).removeChild(divmsg);
            return true;
        }
    },

    "phone": function(e, minLength, maxLength){
        if(/^\+/.test(e.detail)) {
            maxLength++;
            minLength++;
        }
        if(this.length(e, minLength, maxLength) == true) {
            if (!/^\+{0,1}\d{1,}$/.test(e.detail)){
                showErrorMSG(e.target, i18n("PhoneMsg"));
                return false;
            } else return true;
        } else return false;
    },

    "length": function(e, minlength, maxlength) {
        field = e.detail,
        el = e.target,
        parent = el.parentNode;
        if(field.length >= minlength && field.length <= maxlength) {
            hint = parent.lastChild;
            if (parent.lastChild.tagName == "DIV" && parent.lastChild.className == "errorMSG")
                parent.removeChild(hint);
            return true;
        } else {
            text = (minlength == maxlength) ? i18n("CharQuantity") + maxlength
                 : i18n("EnterFrom")+ minlength +i18n("EnterTo")+maxlength+i18n("EnterChars");
            showErrorMSG(el, text);
            return false;
        }
    },

    "min": function(e, min) {
        var field = e.detail.replace(/ /g,"");
        if( parseFloat(field) < min){
            showErrorMSG(e.target, i18n("MinAmount")+min+" "+ i18n(currency));
            return false;
        }else return true;
    },

};

function showErrorMSG(el, msg, elId, position) {
    elId = elId || null;
    position = position || null;
    var parent = el.parentNode;
    if(!elId) {
        if(parent.lastChild.tagName == "DIV" && parent.lastChild.className == "errorMSG")
            parent.removeChild(parent.lastChild);
    }else {
        var divmsg = parent.querySelector("#"+elId) || null;
        if(divmsg != null) (divmsg.parentNode).removeChild(divmsg);
    }
    if(msg != "") {
        el.classList.add("error");
        var div = document.createElement('div');
        div.classList.add("errorMSG");
        if(elId) div.id = elId;
        div.innerHTML = "<p style='float:left; margin:0;width:97%;'>"+msg+"</p>";
        switch(position) {
            case "after": parent.insertBefore(div,el.nextSibling); break;
            default: parent.appendChild(div);
        }
    }
}

function nextByEnter(ev) {
    if (ev.keyCode == 13) {
        var form = ev.target;
        while(true) {
            form = form.parentNode;
            if(form.getAttribute("id") != null && form.getAttribute("id").substring(0, 4) == "form")
              break;
        }
        form.querySelector("a.button.sgreen").click();
        return;
    }
}

function removeAllErrorsFromInput(el) {
    el.classList.remove('error');
    el.style.background = '';
    showErrorMSG(el, ""); }

function fieldsFilter(ev, maxLength, fieldType) {
    var char  = String.fromCharCode(ev.charCode);
    nextByEnter(ev);
    if( (ev.charCode == 0) ) { return true; }
    else if( /[^\d,\.\-\+]/.test(char)) { return false; }
    else{
        var input = ev.target;
        var clearVal = input.value.replace(/ /g,"");
        removeAllErrorsFromInput(input);
        switch(fieldType){
            case   'otp':
            case 'bonus':
            case 'phone':
                if (/[^\d,\+]/.test(char)) return false;                        // первый символ может быть '+'
                if (clearVal.length > 0 && /[^\d]/.test(char)) return false;    // все символы кроме первого должны быть цифрами
                if (input.selectionStart != input.selectionEnd) return true;
                break;
            case 'money':
                if (/[^\d,\.]/.test(char)) return false;
                char = (char==",") ? ".": char;
                if( input.selectionStart != input.selectionEnd && char!=".") return true;
                if( /^0(\.\d{1,2}){0,1}/.test(clearVal) && char == "0" && (input.selectionStart == 0 || input.selectionStart == 1) ) return false;
                if( /^0(\.\d{1,2}){1}/.test(clearVal) && char != "0" && char != "." && (input.selectionStart == 1) ) {
                    input.value = char + clearVal.substring(1);
                    return false; }
                if( /^\d+\.\d{2,}/.test(clearVal) && (input.selectionStart <= clearVal.indexOf(".")) && (clearVal.length < maxLength) && char!="." ) return true;
                if( /^\./.test(clearVal) && (input.selectionStart == 0) && (char == ".") ) {
                    input.value = "0" + clearVal.substring(input.selectionStart);
                    return false; }
                if( (clearVal == "") && (char == ".") ) {
                    input.value = "0"+char;
                    return false; }
                if( /^0$/.test(clearVal) && char != "." ) {
                    input.value+= "."+char;
                    return false; }
                if(/^\d+$/.test(clearVal) && (char == ".") && (clearVal.length< maxLength) ) {
                    input.value = clearVal.substring(0,input.selectionStart) + "." + clearVal.substr(input.selectionStart,2);
                    return false; }
                if(/^\d+\..*\.|\d+\.\d{2,}/.test(clearVal) || (/^\d+\./.test(clearVal) && char == ".") ) return false;
                break;
            case 'date':
                var start = input.selectionStart;
                if( input.type == "date") return true;
                if( /[^\d\-]/.test(char)) return false;
                if( input.selectionStart != input.selectionEnd) return true;
                if( /^\d{4}\-\d{1}\-\d{0,2}$/.test(clearVal) && (start > 4) && (start < clearVal.length-2) && char!="-" ) return true;
                else if( /^\d{4}\-\d{1}\-\d{2}$/.test(clearVal) ) return false;
                if( ((start == 4) || (start == 7) || (start == 6)) && char=="-" && clearVal[start-1]!="-" && clearVal[start]!="-" ) return true;
                else if( ((start != 4) && (start != 7)) && char!="-") {}
                else return false;
                break;
        }
        if(fieldType=='phone' && /^\+/.test(clearVal)) {        // если вводится телефон, и его начали вводить со знака '+'
            if(clearVal.length >= (maxLength + 1)) {
                input.value = clearVal.slice(0,maxLength+1);
                return false;
            }
        } else if(clearVal.length >= maxLength) {
            input.value = clearVal.slice(0,maxLength);
            return false;
        }
    }
}

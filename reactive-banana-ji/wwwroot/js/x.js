$.fn.sendvalue = function(trigger){
  $(this).each(function(){
    var self = this;
    var el = $(self);
    el.keydown(function(e){
      if(e.which == 13) {
        trigger.call(self,el.val());
        return false;
      }
      else
        return true;
    });
  });
};

$.fn.livechange = function(ms,trigger){
  $(this).each(function(){
    var self = this;
    var el = $(self);
    var last_val;
    var check = function(){
      var val = el.val();
      if(val != last_val)
        trigger.call(self);
      last_val = val;
    };
    var checker;
    var restart = function(){
      clearTimeout(checker);
      checker = setInterval(check,ms);
    };
    restart();
    el.keypress(restart).change(restart);
  });
};

(function(){

  ////////////////////////////////////////////////////////////////////////////////
  // State
  var sessionToken = null;
  var global_ids = [], element_count = 0, el_table = [];
  var ji_enable_log = $.cookie('ji_log') == "true";
  var signal_count = 0;

  document.head = document.head || document.getElementsByTagName('head')[0];

  ////////////////////////////////////////////////////////////////////////////////
  // Main entry point
  $(document).ready(function(){
    setTimeout(function(){
      waitForEvents();
    })
  });

  ////////////////////////////////////////////////////////////////////////////////
  // Running instructions

  window.do_logging = function(x){
    $.cookie('ji_log',x.toString());
  };
  
  window.jquery_animate = function(el_id,props,duration,easing,complete){
    var el = lookupElementTable(JSON.parse(el_id));
    $(el).animate(JSON.parse(props),
                  duration * 1,
                  easing * 1,
                  complete);
  }

  window.jquery_scrollToBottom = function(el_id,cont){
    var el = lookupElementTable(JSON.parse(el_id));
    $(el).scrollTop(el.scrollHeight);
    cont();
  };

  window.jquery_setFocus = function(el_id,cont){
    var el = lookupElementTable(JSON.parse(el_id));
    $(el).focus();
    cont();
  };
  
  function waitForEvents(){
    console_log("Polling… (%d signals so far)",signal_count);
    var data = { token: sessionToken };
    var cmd = sessionToken != null? 'poll' : 'init';
    if(cmd == 'init')
      data.info = window.location.href;
    var req = $.ajax({
      dataType: 'json',
      url: cmd,
      data: data,
      success: function(events){
        if(sessionToken == null) {
          sessionToken = req.getResponseHeader('Set-Token').match(/[0-9]+/)*1;
        }
        console_log("Running event" +(events.length>1?'s':'') +"…")
        if(events.length){
          console_log('Event list:');
          runMultipleEvents(events);
        } else {
          runEvent(events,function(){
            waitForEvents();
          });
        }
      },
      error: function(reply){
        console_log("Error, waiting…");
        setTimeout(function(){
          waitForEvents();
        },5000);
      }
    });
  }

  function runMultipleEvents(events){
    if(events.length == 0) {
      return waitForEvents();
    }
    runEvent(events.shift(),function(){
      runMultipleEvents(events);
    });
  }

  function runEvent(event,continuation){
    console_log("Event: %s",JSON.stringify(event));
    for(var key in event){
      switch(key){
      case "EmptyEl": {
        var id = event.EmptyEl;
        var el = lookupElementTable(id);
        // TODO: Drop all the child ids within this element.
        $(el).empty();
        continuation();
        break;
      }
      case "CallDeferredFunction": {
        var call = event.CallDeferredFunction;
        var closure = call[0];
        var theFunction = eval(call[1]);
        var params = call[2];
        theFunction.apply(window, params.concat(function(){
          console_log(this);
          var args = Array.prototype.slice.call(arguments,0);
          signal({
            Event: closure.concat([args])
          },function(){
            // No action.
          });
        }));
        continuation();
        break;
      }
      case "CallFunction": {
        var call = event.CallFunction;
        var theFunction = eval(call[0]);
        var params = call[1];
        theFunction.apply(window, params.concat(function(){
          var args = Array.prototype.slice.call(arguments,0);
          signal({
            FunctionCallValues: args
          },function(){
            continuation();
          });
        }));
        break;
      }
      case "Delete": {
        event_delete(event);
        continuation();
        break;
      }
      case "Debug": {
        if(window.console)
          console.log("Server debug: %o",event.Debug);
        continuation();
        break;
      }
      case "Clear": {
        $('body').empty();
        continuation();
        break;
      }
      case "GetElementsByTagName": {
        var elements = document.getElementsByTagName(event.GetElementsByTagName);
        var els = [];
        var len = elements.length;
        for(var i = 0; i < len; i++) {
          els.push({
            Element: getElementGuid(elements[i])
          });
        }
        signal({
          Elements: els
        },function(){
          continuation();
        });
        break;
      }
      case "SetStyle": {
        var set = event.SetStyle;
        var id = set[0];
        var style = set[1];
        var el = lookupElementTable(id);
        var len = style.length;
        for(var i = 0; i < len; i++){
          el.style[style[i][0]] = style[i][1];
        }
        continuation();
        break;
      }
      case "SetAttr": {
        var set = event.SetAttr;
        var id = set[0];
        var key = set[1];
        var value = set[2];
        var el = lookupElementTable(id);
        $(el).attr(key,value);
        continuation();
        break;
      }
      case "GetValue": {
        var id = event.GetValue;
        var el = lookupElementTable(id);
        var value = $(el).val();
        signal({
          Value: value
        },function(){
          continuation();
        });
        break;
      }
      case "GetLocation": {
        signal({
          Location: window.location.href
        },function(){
          continuation();
        });
        break;
      }
      case "GetValues": {
        var ids = event.GetValues;
        var len = ids.length;
        var values = [];
        for(var i = 0; i < len; i++) {
          values.push($(lookupElementTable(ids[i])).val());
        }
        signal({
          Values: values
        },function(){
          continuation();
        });
        break;
      }
      case "Append": {
        var append = event.Append;
        $(lookupElementTable(append[0])).append($(lookupElementTable(append[1])));
        continuation();
        break;
      }
      case "SetText": {
        var set = event.SetText;
        $(lookupElementTable(set[0])).text(set[1]);
        continuation();
        break;
      }
      case "SetTitle": {
        document.title = event.SetTitle;
        continuation();
        break;
      }
      case "SetHtml": {
        var set = event.SetHtml;
        $(lookupElementTable(set[0])).html(set[1]);
        continuation();
        break;
      }
      case "Bind": {
        var bind = event.Bind;
        var eventType = bind[0];
        var handlerGuid = bind[2];
        var el = lookupElementTable(bind[1]);
        console_log('event type: ' + eventType);
        if(eventType == 'livechange') {
          $(el).livechange(300,function(e){
            signal({
              Event: handlerGuid.concat([[$(el).val()]])
            },function(){
              // no action
            });
            return true;
          });
        } else if(eventType == 'sendvalue') {
          $(el).sendvalue(function(x){
            signal({
              Event: handlerGuid.concat([[x]])
            },function(){});
          });
        }
        else {
          $(el).bind(eventType,function(e){
            signal({
              Event: handlerGuid.concat([e.which?[e.which.toString()]:[]])
            },function(){
              // no action
            });
            return true;
          });
        }
        continuation();
        break;
      }
      default: continuation();
      }
    }
  }

  function event_delete(event){
    var id = event.Delete;
    var el = lookupElementTable(id);
    // TODO: Drop all the child ids within this element.
    $(el).empty();
    $(el).remove();
    deleteElementTable(id);
  }

  ////////////////////////////////////////////////////////////////////////////////
  // Signalling events

  function signal(signal,continuation){
    signal_count++;
    console_log('Signal: %s',JSON.stringify(signal));
    $.ajax({
      dataType: 'json',
      url:'signal',
      data: { token: sessionToken, signal: JSON.stringify(signal) },
      success: function(){
        continuation();
      },
      error: function(reply){
        console_log("Error: %o",reply);
      }
    });
  }

  function lookupElementTable(elid){
    if(elid == 'body')
      return document.body;
    else if(elid == 'head')
      return document.head;
    else if(el_table[elid]){
      return el_table[elid];
    } else {
      if(elid[0] == '*'){
        var create = elid.split(':');
        var el = document.createElement(create[1]);
        el_table[elid] = el;
        return el;
      } else {
        throw "Unknown element: " + elid;
      }
    }
  }
  function deleteElementTable(elid){
    delete el_table[elid];
  }
  
  // Get/generate a guid for an element
  function getElementGuid(element){
    if(element.id) return element.id;
    else {
      if(global_ids.length > 0) {
        var id = global_ids.pop();
        element.id = id;
        el_table[id] = element;
        return id;
      } else {
        var id = element_count.toString();
        element.id = id;
        el_table[id] = element;
        element_count++;
        return id;
      }
    }
  }

  // A log
  function console_log(){
    if (ji_enable_log) {
      window.console.log.apply(window.console,arguments);
    }
  };

})();

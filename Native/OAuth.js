var _tiziano88$elm_oauth$Native_OAuth = function() {

  function initAuthFlow(url) {
    return _elm_lang$core$Native_Scheduler.nativeBinding(function(callback) {
      var w = window.open(url);

      function checkHash() {
        try {
          var hash = w.location.hash;
          if (hash) {
            callback(_elm_lang$core$Native_Scheduler.succeed(hash));
            w.close();
          }
        } catch (err) {
          setTimeout(checkHash, 100);
        }
      }
      setTimeout(checkHash, 100);
    });
  }

  return {
    initAuthFlow : initAuthFlow,
  };

}();

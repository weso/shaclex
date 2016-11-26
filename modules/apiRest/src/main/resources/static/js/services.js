var dataServices = angular.module('dataServices', ['ngResource']);

dataServices.factory('Data', ['$resource',
  function($resource) {
    return $resource('data/:dataname', {}, {
      query: {method:'GET', params:{dataname:'all'},isArray:true}
    });
  }]);

// A simple websocket service
dataServices.service('WS', ['$rootScope', function($scope) {
	
	// Private WebSocket object
	var ws = null;

	this.connect = function (addr, cb) {
	    console.debug("Staring websocket");

		ws = new WebSocket(addr);
		if (typeof(cb) != 'undefined') {
			ws.onopen = cb;
		}

		ws.onmessage = function(msg) { console.log('Ignored message: ' + msg); };
		ws.onclose = this.onClose;
	};

	this.sendText = function (msg) { ws.send(msg); };

	this.setCallback = function(f) { ws.onmessage = f; };

	this.onClose = function() { console.log("Closing WebSocket.")}
}]);


var mainApp = angular.module('mainApp', ['dataServices']);

// Deals with navigating the menu
mainApp.controller('navController', ['$scope', '$location', function($scope, $location) {
    $scope.navClass = function (page) {
        var currentRoute = $location.path().substring(1) || 'home';
        return page === currentRoute ? 'active' : '';
    };
}]);

mainApp.controller('PhoneListCtrl', ['$scope', 'Data', function ($scope, Data) {
  
  $scope.phones = Data.query();
  $scope.stuff = "Things.";

  $scope.queryChange = function() {
    $scope.$watch('query', function (query) {
      setTimeout(function() {
        if (query == $scope.query) {
          console.log('Searching for: ' + query)
          if (query == '') $scope.phones = Data.query();
          else $scope.phones = Data.query({'dataname': $scope.query});
        }
      }, 500);
    });
  };
}]);

mainApp.controller('xhrController', ['$scope', '$http', function($scope, $http) {
  
    $scope.data = 'Nothing yet';

    $scope.queryChange = function() {
        if ($scope.query != '') {
            $http.get('/things/' + $scope.query).success(function(data) {
            $scope.data = 'Your data: ' + data;
        });
        } else {
            $scope.data = '';
        }
    };
}]);

mainApp.controller('wsChatController', ['$scope', '$location', 'WS', function($scope, $location, WS) {
    console.log('Starting chat session');
    var port = ':' + $location.port();
    if ($location.port() == '') {
        port = '';
    }

    var address = 'ws://' + $location.host() + port + '/wschat';
    $scope.word = /^\s*\w*\s*$/;

    // have to count the messages
    var msgnum = 0;
    $scope.messages = [];

    function pushmsg(msg) {
        console.log('Received message: ' + msg.data);

        $scope.messages.push({id: msgnum++, time: (new Date()).toTimeString().split(' ')[0], msg: msg.data});
        if ($scope.messages.length > 10) {
            $scope.messages.splice(0,1);
        }
    }

    $scope.needJoin = true;
    $scope.chatName = '';
    $scope.outmsg = '';

    $scope.join = function() {
        console.log($scope.chatName + ' is joining.');

        if ($scope.chatName != '') {
            WS.connect(address + '/' + $scope.chatName, function () {

                $scope.needJoin = false;

                $scope.send = function() {
                    var data = $scope.outmsg;
                    if (data != '') {
                        $scope.outmsg = '';
                        WS.sendText(data);
                    }
                };

                WS.setCallback(function(msg) {
                    $scope.$apply(function() {
                        pushmsg(msg);
                    });
                });

                WS.onClose = function() {
                    $scope.send = function() {
                        pushmsg('Disconnected!')
                    };
                };

            });
        }


    };

    $scope.send = function() {
        console.log('Not connected!');
    };
}]);

mainApp.controller('websocketController', ['$scope', '$location', 'WS', function($scope, $location, WS) {
    console.log($location.host() + ':' + $location.port());
    var port = ':' + $location.port();
    if ($location.port() == '') {
        port = '';
    }
    var address = 'ws://' + $location.host() + port + '/websocket';
    $scope.wsin = '';
    $scope.wsout = [];
    $scope.active = true;

    $scope.buttonmsg = "Click To Begin";

    $scope.messageChange = function() {
        WS.sendText($scope.wsin);
        $scope.wsin = '';
    };

    $scope.begin = function() {
        // Disable clicking the button again
        $scope.begin = function() {
            alert("WebSocket already connecting!");
        };

        $scope.buttonmsg = "Connecting...";

        WS.connect(address, function() {
            $scope.active = false;

            WS.setCallback(function(msg) {
                $scope.$apply(function() {
                    $scope.wsout.push('Received: ' + msg.data);
                    if ($scope.wsout.length > 10) {
                        $scope.wsout.splice(0,1);
                    }
                });
            });

            WS.onClose = function() {
                $scope.wsout = ['WebSocket Closed!'];
            };
        });
    };

}]);

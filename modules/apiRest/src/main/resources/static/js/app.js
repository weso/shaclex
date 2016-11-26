var phonecatApp = angular.module('phonecatApp', [
  'ngRoute',
  'mainApp'
]);

phonecatApp.config(['$routeProvider',
  function($routeProvider) {
    $routeProvider.
      when('/home', {
        templateUrl: '/twirl-home'  // a twirl template
      }).
      when('/phones', {
        templateUrl: 'partials/phones.html',
        controller: 'PhoneListCtrl'
      }).
      when('/xhr', {
        templateUrl: 'partials/xhr.html',
        controller: 'xhrController'
      }).
      when('/websocket', {
        templateUrl: 'partials/websocket.html',
        controller: 'websocketController'
      }).
      when('/wschat', {
        templateUrl: 'partials/chat.html',
        controller: 'wsChatController'
      })
      .otherwise({
        redirectTo: '/home'
      });
  }]);

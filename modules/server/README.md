# Server module

This module implements a simple server based on the [http4s](http://http4s.org/) library.

The server contains a simple REST API defined [here](https://github.com/labra/shaclex/blob/master/modules/server/src/main/scala/es/weso/server/APIService.scala) and a web service that calls the REST API. 
The web service has been implemented using [Twirl templates](https://www.playframework.com/documentation/2.6.x/ScalaTemplates) 
which are defined [in this folder](https://github.com/labra/shaclex/tree/master/modules/server/src/main/twirl/es/weso). 
Some parts of the web service are implemennted in plain Javascript [here](https://github.com/labra/shaclex/tree/master/modules/server/src/main/resources/staticviews/js). 
In the future, it would be better to replace that Javascript code by ScalaJs.

The server is deployed at [shaclex](http://shaclex.validatingrdf.com).
package es.weso.server

import cats.effect.Effect
import org.http4s.HttpService
import org.http4s.server.staticcontent
import org.http4s.server.staticcontent.ResourceService.Config

object Http4sUtils {

  def staticResource[F[_]: Effect](config: Config[F]): HttpService[F] = {
    // Remove cache for development
    // val cachedConfig: Config[IO] = config.copy(cacheStrategy = staticcontent.MemoryCache())
    val cachedConfig: Config[F] = config.copy(cacheStrategy = staticcontent.NoopCacheStrategy[F])
    staticcontent.resourceService(cachedConfig)
  }

}
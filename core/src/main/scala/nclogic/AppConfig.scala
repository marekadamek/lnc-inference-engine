package nclogic

import com.typesafe.config.{Config, ConfigFactory}

trait AppConfig {
  lazy val config: Config = ConfigFactory.load()
}

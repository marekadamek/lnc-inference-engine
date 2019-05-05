package lnc

import com.typesafe.config.{Config, ConfigFactory}

trait AppConfig {
  lazy val config: Config = ConfigFactory.load()
}

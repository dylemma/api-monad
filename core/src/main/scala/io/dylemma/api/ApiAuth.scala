package io.dylemma.api

trait ApiAuth {
	def authenticatedUser: Option[String]
	def hasPermission(permission: String): Boolean
}

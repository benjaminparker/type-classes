import java.util.UUID

object CacheUpdate {

  trait Identifiable {
    def id: String = UUID.randomUUID().toString
  }

  trait Cacheable[T] {
    def save(id: String, t: T): Either[String, T]

    def find(id: String): Either[String, T]
  }

  case class Declaration(warehouseInfo: WarehouseInfo, officeOfExit: OfficeOfExit) extends Identifiable
  case class WarehouseInfo(warehouseName: String) extends Identifiable
  case class OfficeOfExit(officeName: String) extends Identifiable

  trait DeclarationCache[I <: Identifiable] {

    var store: Map[String, I] = Map.empty

    def findInCache(id: String): Either[String, I] = store.get(id) match {
      case Some(foundItem) => Right(foundItem)
      case None => Left("Not found")
    }

    def storeInCache(i: I) = {
      store = store + (i.id -> i)
      i
    }
  }

  object CacheableThings {

    implicit object CacheableWarehouseInfo extends Cacheable[WarehouseInfo] with DeclarationCache[Declaration] {
      def save(id: String, w: WarehouseInfo): Either[String, WarehouseInfo] = {
        for {
          declaration <- findInCache(id)
          _ = storeInCache(declaration.copy(warehouseInfo = w))
        } yield w
      }

      def find(id: String): Either[String, WarehouseInfo] = {
        for {
          declaration <- findInCache(id)
        } yield declaration.warehouseInfo
      }
    }

    implicit object CacheableOfficeOfExit extends Cacheable[OfficeOfExit] with DeclarationCache[Declaration] {
      def save(id: String, o: OfficeOfExit): Either[String, OfficeOfExit] = {
        for {
          declaration <- findInCache(id)
          _ = storeInCache(declaration.copy(officeOfExit = o))
        } yield o
      }

      def find(id: String): Either[String, OfficeOfExit] = {
        for {
          declaration <- findInCache(id)
        } yield declaration.officeOfExit
      }
    }
  }

  object UpdateService {
    def retrieveSomething[T: Cacheable](id: String): Either[String, T] = {
      implicitly[Cacheable[T]].find(id)
    }

    def updateSomething[T: Cacheable](id: String, t: T): Either[String,T] = {
      implicitly[Cacheable[T]].save(id, t)
    }
  }

  import CacheableThings._
  val myWarehouse = WarehouseInfo("GB3378ASDW")
  UpdateService.updateSomething("someId", myWarehouse)

  val myOffice = OfficeOfExit("Birmingham Airport")
  UpdateService.updateSomething("someId", myOffice)

  UpdateService.retrieveSomething[WarehouseInfo]("someId") match {
    case Right(warehouseInfo) => displayWarehouse(warehouseInfo)
    case Left(msg) => error(msg)
  }

  UpdateService.retrieveSomething[OfficeOfExit]("someId") match {
    case Right(officeOfExit) => displayOfficeOfExit(officeOfExit)
    case Left(msg) => error(msg)
  }

  def displayWarehouse(warehouseInfo: WarehouseInfo): String = s"Here is my warehouse: ${warehouseInfo.warehouseName}"
  def displayOfficeOfExit(officeOfExit: OfficeOfExit): String = s"Here is my office of exit: ${officeOfExit.officeName}"

  def error(msg: String): String = s"I got an error with message: $msg"
}
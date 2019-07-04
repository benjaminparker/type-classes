import java.util.UUID

object CacheUpdate {

  trait Identifiable {
    def id: String = UUID.randomUUID().toString
  }

  trait Cacheable[T <: Identifiable] {
    def save(t: T): T

    def find(id: String): Either[String, T]
  }

  case class WarehouseInfo(warehouseName: String) extends Identifiable
  case class OfficeOfExit(officeName: String) extends Identifiable

  trait DeclarationCache[T <: Identifiable] {

    var store: Map[String, T] = Map.empty

    def findInCache(id: String): Either[String, T] = Right(store.getOrElse(id, Left("Cannot find it")))

    def storeInCache(t: T) = {
      store = store + (t.id -> t)
      t
    }
  }

  object CacheableThings {

    implicit object CacheableWarehouseInfo extends Cacheable[WarehouseInfo] with DeclarationCache[WarehouseInfo] {
      def save(w: WarehouseInfo): WarehouseInfo = storeInCache(w)
      def find(id: String): Either[String, WarehouseInfo] = findInCache(id)
    }

    implicit object CacheableOfficeOfExit extends Cacheable[OfficeOfExit] with DeclarationCache[OfficeOfExit] {
      def save(o: OfficeOfExit): OfficeOfExit = storeInCache(o)
      def find(id: String): Either[String, OfficeOfExit] = findInCache(id)
    }

  }

  object UpdateService {
    def retrieveSomething[T: Cacheable](id: String): Either[String, T] = {
      implicitly[Cacheable[T]].find(id)
    }

    def updateSomething[T: Cacheable](t: T): T = {
      implicitly[Cacheable[T]].save(t)
    }
  }

  import CacheableThings._
  val myWarehouse = WarehouseInfo("GB3378ASDW")
  UpdateService.updateSomething(myWarehouse)

  val myOffice = OfficeOfExit("Birmingham Airport")
  UpdateService.updateSomething(myOffice)

  UpdateService.retrieveSomething("ABCD1234") match {
    case Right(warehouseInfo) => displayWarehouse(warehouseInfo)
    case Right(officeOfExit) => displayOfficeOfExit(officeOfExit)
    case Left(msg) => error(msg)
  }

  def displayWarehouse(warehouseInfo: WarehouseInfo): String = s"Here is my warehouse: ${warehouseInfo.warehouseName}"
  def displayOfficeOfExit(officeOfExit: OfficeOfExit): String = s"Here is my office of exit: ${officeOfExit.officeName}"

  def error(msg: String): String = s"I got an error with message: $msg"
}
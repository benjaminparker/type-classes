import java.util.UUID

object CacheUpdate extends App {

  trait Identifiable {
    def id: String
  }

  trait Cacheable[T] {
    def save(id: String, t: T): T

    def find(id: String): Either[String, T]
  }

  case class Declaration(id: String =  UUID.randomUUID().toString, warehouseInfo: Option[WarehouseInfo] = None, officeOfExit: Option[OfficeOfExit] = None) extends Identifiable

  case class WarehouseInfo(warehouseName: String)

  case class OfficeOfExit(officeName: String)

  object DeclarationCache {

    var store: Map[String, Declaration] = {
      println("New STORE...")
      Map.empty
    }

    def findInCache(id: String): Either[String, Declaration] = store.get(id) match {
      case Some(foundItem) => Right(foundItem)
      case None => Left("Not found")
    }

    def storeInCache(i: Declaration) = {
      store = store + (i.id -> i)
      println(s"STORE: $store")

      i
    }
  }

  object CacheableThings {

    import DeclarationCache._

    implicit object CacheableDeclaration extends Cacheable[Declaration] {
      def save(id: String, w: Declaration): Declaration = {
        storeInCache(declaration)
        declaration
      }

      def find(id: String): Either[String, Declaration] = {
        DeclarationCache.findInCache(id)
      }
    }

    implicit object CacheableWarehouseInfo extends Cacheable[WarehouseInfo] {
      def save(id: String, w: WarehouseInfo): WarehouseInfo = {
        findInCache(id) match {
          case Right(declaration) => storeInCache(declaration.copy(warehouseInfo = Some(w)))
          case Left(_) => storeInCache(Declaration(warehouseInfo = Some(w)))
        }
        w
      }

      def find(id: String): Either[String, WarehouseInfo] = {
        DeclarationCache.findInCache(id) match {
          case Right(Declaration(_, Some(warehouseInfo), _)) => Right(warehouseInfo)
          case _ => Left("Warehouse Info Not found")
        }
      }
    }

    implicit object CacheableOfficeOfExit extends Cacheable[OfficeOfExit] {
      def save(id: String, o: OfficeOfExit): OfficeOfExit = {
        findInCache(id) match {
          case Right(declaration) => storeInCache(declaration.copy(officeOfExit = Some(o)))
          case Left(_) => storeInCache(Declaration(officeOfExit = Some(o)))
        }
        o
      }

      def find(id: String): Either[String, OfficeOfExit] = {
        findInCache(id) match {
          case Right(Declaration(_, _, Some(officeOfExit))) => Right(officeOfExit)
          case _ => Left("Office of Exit Not found")
        }
      }

    }

  }

  object UpdateService {
    def retrieveSomething[T: Cacheable](id: String): Either[String, T] = {
      implicitly[Cacheable[T]].find(id)
    }

    def updateSomething[T: Cacheable](id: String, t: T): T = {
      implicitly[Cacheable[T]].save(id, t)
    }
  }

  import CacheableThings._

  val declaration = Declaration()
  UpdateService.updateSomething(declaration.id, declaration)

  val myWarehouse = WarehouseInfo("GB3378ASDW")
  UpdateService.updateSomething(declaration.id, myWarehouse)

  val myOffice = OfficeOfExit("Birmingham Airport")
  UpdateService.updateSomething(declaration.id, myOffice)

  UpdateService.retrieveSomething[WarehouseInfo](declaration.id) match {
    case Right(warehouseInfo) => displayWarehouse(warehouseInfo)
    case Left(msg) => error(msg)
  }

  UpdateService.retrieveSomething[OfficeOfExit](declaration.id) match {
    case Right(officeOfExit) => displayOfficeOfExit(officeOfExit)
    case Left(msg) => error(msg)
  }

  def displayWarehouse(warehouseInfo: WarehouseInfo) = println(s"Here is my warehouse: ${warehouseInfo.warehouseName}")

  def displayOfficeOfExit(officeOfExit: OfficeOfExit) = println(s"Here is my office of exit: ${officeOfExit.officeName}")

  def error(msg: String) = println(s"I got an error with message: $msg")
}
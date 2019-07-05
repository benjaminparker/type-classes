package persistance

import model.{Cacheable, ExportsDeclaration, OfficeOfExit, WarehouseInfo}

trait CacheableThings {

  import persistance.DeclarationStore._

  implicit object CacheableDeclaration extends Cacheable[ExportsDeclaration] {

    def save(id: String, declaration: ExportsDeclaration): ExportsDeclaration = {
      storeInCache(declaration)
      declaration
    }

    def find(id: String): Either[String, ExportsDeclaration] = findInCache(id)
  }

  implicit object CacheableWarehouseInfo extends Cacheable[WarehouseInfo] {

    def save(id: String, warehouseInfo: WarehouseInfo): WarehouseInfo = {
      findInCache(id) match {
        case Right(declaration) => storeInCache(declaration.copy(warehouseInfo = Some(warehouseInfo)))
        case Left(_) => storeInCache(ExportsDeclaration(warehouseInfo = Some(warehouseInfo)))
      }
      warehouseInfo
    }

    def find(id: String): Either[String, WarehouseInfo] = {
      findInCache(id) match {
        case Right(ExportsDeclaration(_, Some(warehouseInfo), _)) => Right(warehouseInfo)
        case _ => Left("Warehouse Info Not found")
      }
    }
  }

  implicit object CacheableOfficeOfExit extends Cacheable[OfficeOfExit] {
    def save(id: String, o: OfficeOfExit): OfficeOfExit = {
      findInCache(id) match {
        case Right(declaration) => storeInCache(declaration.copy(officeOfExit = Some(o)))
        case Left(_) => storeInCache(ExportsDeclaration(officeOfExit = Some(o)))
      }
      o
    }

    def find(id: String): Either[String, OfficeOfExit] = {
      findInCache(id) match {
        case Right(ExportsDeclaration(_, _, Some(officeOfExit))) => Right(officeOfExit)
        case _ => Left("Office of Exit Not found")
      }
    }
  }
}

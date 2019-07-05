package model

import java.util.UUID

case class WarehouseInfo(warehouseName: String)

case class OfficeOfExit(officeName: String)

case class ExportsDeclaration(id: String =  UUID.randomUUID().toString, warehouseInfo: Option[WarehouseInfo] = None, officeOfExit: Option[OfficeOfExit] = None)


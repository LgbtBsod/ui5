@AbapCatalog.sqlViewName: 'ZICHKCRESSQL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Check: Result code reference (customizing)'

// ResultCode "X" = удовлетворительно, "" = неудовлетворительно — тот же
// код, что ext/util/Constants.js RESULT_CODES.SATISFACTORY на клиенте и
// serve_config.py RESULT_CODE_SATISFACTORY на моке. Здесь константа не
// нужна — это ДАННЫЕ (строка таблицы), не литерал в коде; единственное
// место, которое обязано с ней синхронизироваться — ZCL_CHECK_DPC_EXT
// (агрегация ChecksSuccess/BarriersSuccess, см. его комментарий
// c_result_satisfactory).
define view ZI_CheckResult
  as select from zchk_cres as CheckResult
{
  key CheckResult.result_code as ResultCode,

      @Search.defaultSearchElement: true
      CheckResult.result_text as ResultText
}

@AbapCatalog.sqlViewName: 'ZICHKCTYPSQL'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Check: Check type reference (customizing)'

// Справочник типов проверок — таблица ведения (customizing), а не
// транзакционные данные: правьте через SM30/генерируемый maintenance-view
// над ZCHK_CTYPE, не через это CDS-представление (оно read-only по
// построению — нет @ObjectModel.createEnabled и др.).
//
// PkLevels — CSV-список применимых уровней КПР ("0,1,2,3,4"). Это
// СОЗНАТЕЛЬНОЕ упрощение модели данных, унаследованное от мока
// (serve_config.py REFERENCE_DATA) — оно и есть причина, почему
// контекстный F4-фильтр "тип проверки применим к текущему LpcKey"
// НЕ выражается через Common.ValueListParameterFilterOnly (та умеет
// только точное равенство, не "CSV содержит X") и остаётся на клиенте
// (см. ext/util/SubTableCrud.js, PkLevels Contains-фильтр). Правильное
// решение для реального проекта — вынести PkLevels в отдельную
// M:N-таблицу ZCHK_CTYPE_PKL(check_type_code, pk_level) и добавить сюда
// association к ней; тогда контекстный фильтр становится выразим через
// ValueListParameterFilterOnly на ассоциацию. Не сделано здесь, чтобы не
// расходиться с уже реализованной (и протестированной) моделью мока —
// см. abap/README.md, раздел "Известные компромиссы".
define view ZI_CheckType
  as select from zchk_ctype as CheckType
{
  key CheckType.check_type_code as CheckTypeCode,

      @Search.defaultSearchElement: true
      @Search.fuzzinessThreshold: 0.8
      CheckType.check_type_text as CheckTypeText,

      @Search.defaultSearchElement: true
      CheckType.category        as Category,

      CheckType.pk_levels        as PkLevels
}

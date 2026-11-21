# KingbaseES SQL Server 模式测试

适配入口为 `KingbaseSqlServerSchemaMetadata`，方言 ID 为 `kingbase-sqlserver`。
DDL、查询、分页、批量 upsert、枚举以及 JSON 条件继承原生 SQL Server 实现，
差异通过本目录对应的 Kingbase 类处理。

## 实库验证结果（2026-11-10）

- Kingbase：`127.0.0.1:54323/test`，`system`，实测版本 `KingbaseES V009R004C019`，
  `show database_mode` 为 `sqlserver`。
- R2DBC：实际加载 `r2dbc-kingbase-main-df2fc01d26-1.jar`。
- JDBC：金仓安装目录提供的 `kingbase8-9.0.0.jar`，驱动报告版本 `V009R004C019`。
- 原生 SQL Server：仓库默认的 `mcr.microsoft.com/mssql/server:2017-latest-ubuntu`，
  通过 Testcontainers 启动临时实例执行同一组测试。
- Kingbase 40 项单元测试、47 项实库测试，原生 SQL Server 31 项实库测试及
  19 项原生单元/方言测试，共 137 项通过，无失败、无错误、无跳过。

所有 Kingbase 实库套件均创建独立临时 Schema，结束后删除 Schema 及其对象。
不使用 MySQL/Oracle 模式模拟 SQL Server 模式。
SQL Server 模式必须提供外部 Kingbase 实例，现有 MySQL/Oracle 容器镜像不支持此模式。

## 官方及原生测试对齐方式

此处官方指 EasyORM 原生测试，不把补充测试标为官方测试。
通用套件直接继承原生基类，不覆盖测试方法或断言；元数据测试保留原生全部断言，
只替换连接、Schema、解析器，并将硬编码 `dbo` 改为连接默认的隔离 Schema。

| Kingbase 入口 | 数量 | 对齐来源及运行方式 |
|---|---:|---|
| `KingbaseSqlServerOfficialSyncTests` | 11 | `MSSQLBasicTest` 使用的 `BasicCommonTests`，同步 ORM API，经 R2DBC 同步桥接执行 |
| `KingbaseSqlServerOfficialJdbcSyncTests` | 11 | 同一 `BasicCommonTests`，通过金仓 JDBC 驱动和 `TestSyncSqlExecutor` 执行 |
| `KingbaseSqlServerOfficialReactiveTests` | 13 | `MSSQLReactiveTests` 使用的 `BasicReactiveTests`，R2DBC |
| `KingbaseSqlServerOfficialBatchUpsertTests` | 4 | `MSSQLBatchUpsertIntegrationTest` 使用的 `AbstractBatchUpsertIntegrationTest` |
| `KingbaseSqlServerOfficialTableMetadataTests` | 1 | `MSSQLTableMetaParserTest.testParse`，固定类型、长度、精度、主键和非空断言不变 |
| `KingbaseSqlServerOfficialJsonTests` | 2 | `SqlServerJsonQueryIntegrationTest` 使用的 `AbstractJsonQueryIntegrationTest`，保留 strict 模式及查询结果断言 |
| `KingbaseSqlServerR2dbcIntegrationTests` | 5 | 补充检查实际模式/驱动、中文与单引号注释、尺寸及 MAX 类型、索引标志/顺序、Schema 隔离和实际重复键转换 |

原生 SQL Server 基线运行 `MSSQLBasicTest`、`MSSQLReactiveTests`、
`MSSQLBatchUpsertIntegrationTest`、`MSSQLTableMetaParserTest`、
`SqlServerJsonQueryIntegrationTest`，共 31 项，保留原文件和断言。

单元测试中，upsert 的 10 项及分页的 3 项保留原生对应测试全部断言；
元数据和 LIKE 使用原生固定预期，其余为差异点回归测试。

## 实库发现并修正的差异

1. `datetime2` 经 R2DBC 返回带 7 位小数秒的字符串；列编解码器先转换为 `Timestamp`，
   再委托原生日期编解码器，保留枚举和 JSON 列监听器。
2. SQL Server 注释使用的多语句预编译会触发驱动参数编号错误；建表/改表继承原生构造器，
   注释单独使用 `COMMENT ON`，转义单引号，列删除时由数据库清理其注释。
3. SQL Server 模式将方括号解释为标识符，无法执行 PostgreSQL 索引查询中的数组下标；
   索引解析改为继承原生 SQL Server 解析器，使用 `sys.indexes/index_columns`，
   单独处理 `0/1` 标志和升降序，排除非键列。
4. `information_schema` 缺少部分 NVARCHAR 长度和数值精度；从 `sys.columns` 补全，
   NVARCHAR/NCHAR 按字符而非字节计算长度，保留 `varchar/nvarchar/varbinary(max)`。
   列注释通过 `col_description` 读取。
5. 当前实库未提供 `JSON_VALUE/JSON_QUERY`；JSON 条件继承原生 builder，
   替换为 `jsonb_extract_path/jsonb_extract_path_text`，路径分段绑定参数，
   保留原生 JSON null 的存在性语义。
6. 同步桥接包装的重复键异常解包后转换为 `DuplicateKeyException`，其他异常保持原样。
7. 官方公共解析器 `RDBTableMetadataParser` 保持原样。其响应式表注释查询使用
   `singleMap()`，该包装器返回 false 中断迭代，响应式执行器因而不发出注释行。
   在 `KingbaseSqlServerTableMetadataParser` 中单独覆盖 `parseByNameReactive`，
   使用 `RecordResultWrapper` 顺序读取列、注释及索引，每类查询只执行一次。
   缺表仍返回空结果，无注释时保留 null；不修改官方公共类或原生 SQL Server 实现。

## 运行

只运行 40 项单元测试，无需数据库：

```bash
mvn -o -Pkingbase-r2dbc -pl hsweb-easy-orm-rdb -am \
  '-Dtest=KingbaseSqlServer*Test' \
  -Dsurefire.failIfNoSpecifiedTests=false test
```

运行 Kingbase 单元测试及全部实库测试（87 项）：

```bash
export KINGBASE_SQLSERVER_R2DBC_URL='r2dbc:kingbase://127.0.0.1:54323/test?mode=sqlserver'
export KINGBASE_SQLSERVER_R2DBC_USERNAME=system
export KINGBASE_SQLSERVER_R2DBC_PASSWORD='<password>'
export KINGBASE_SQLSERVER_JDBC_URL='jdbc:kingbase8://127.0.0.1:54323/test'
export KINGBASE_SQLSERVER_JDBC_DRIVER='/opt/kingbaseSqlserver/KESRealPro/V009R004C019/Interface/jdbc/kingbase8-9.0.0.jar'
mvn -o -Pkingbase-r2dbc -pl hsweb-easy-orm-rdb -am \
  '-Dtest=KingbaseSqlServer*Test,KingbaseSqlServer*Tests' \
  -Dsurefire.failIfNoSpecifiedTests=false \
  -Dkingbase.sqlserver.r2dbc.integration=true test
```

JDBC jar 使用独立类加载器，不要求将本机安装文件加入项目依赖。
未配置 JDBC jar 时 JDBC 套件会明确跳过；本次验证配置了该路径，无跳过。

连同原生 SQL Server 实库和原生单元/方言基线一起运行（需要 Docker，首次会拉取 SQL Server 镜像）：

```bash
mvn -o -Pkingbase-r2dbc -pl hsweb-easy-orm-rdb -am \
  '-Dtest=KingbaseSqlServer*Test,KingbaseSqlServer*Tests,MSSQLBasicTest,MSSQLReactiveTests,MSSQLBatchUpsertIntegrationTest,MSSQLTableMetaParserTest,SqlServerJsonQueryIntegrationTest,SqlServerBatchUpsertOperatorTest,SqlServer2012PaginatorBranchTest,DialectTest' \
  -Dsurefire.failIfNoSpecifiedTests=false \
  -Dkingbase.sqlserver.r2dbc.integration=true test
```

结果见 `hsweb-easy-orm-rdb/target/surefire-reports/TEST-*.xml`。
金仓兼容范围参考：[数据类型](https://help.kingbase.com.cn/v9.4.12/development/application-develop-guide/kes-vs-sqlserver/kes-vs-sqlserver-1.html)、
[兼容视图](https://help.kingbase.com.cn/v9.4.12/development/application-develop-guide/kes-vs-sqlserver/kes-vs-sqlserver-3.html)。

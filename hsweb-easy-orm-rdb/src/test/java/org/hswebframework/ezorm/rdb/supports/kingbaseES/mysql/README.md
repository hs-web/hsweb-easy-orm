# Kingbase MySQL 测试依据

官方指 hs-web/hsweb-easy-orm 上游 `4.2`，本次核对提交为
`96b61b0b8e2f2cd9fc61f9fcc09f73c26ccd19db`。本仓库下列原生测试与该提交一致。

| Kingbase 测试入口 | 官方来源 | 断言约束 |
|---|---|---|
| `KingbaseMysqlOfficialSyncTests` | [BasicCommonTests](https://github.com/hs-web/hsweb-easy-orm/blob/96b61b0b8e2f2cd9fc61f9fcc09f73c26ccd19db/hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/BasicCommonTests.java) | 直接继承 11 个测试方法，不覆盖测试或断言 |
| `KingbaseMysqlOfficialReactiveTests` | [BasicReactiveTests](https://github.com/hs-web/hsweb-easy-orm/blob/96b61b0b8e2f2cd9fc61f9fcc09f73c26ccd19db/hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/BasicReactiveTests.java) | 直接继承 13 个测试方法，不覆盖测试或断言 |
| 同上 `testExceptionTrans` | [MysqlReactiveTests](https://github.com/hs-web/hsweb-easy-orm/blob/96b61b0b8e2f2cd9fc61f9fcc09f73c26ccd19db/hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/mysql/MysqlReactiveTests.java) | 保留原测试流程及 DuplicateKeyException 预期 |
| `KingbaseMysqlOfficialTableMetadataTests.testParse` | [MysqlTableMetaParserTest](https://github.com/hs-web/hsweb-easy-orm/blob/96b61b0b8e2f2cd9fc61f9fcc09f73c26ccd19db/hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/mysql/MysqlTableMetaParserTest.java) | 保留测试流程及其他断言；INT 精度按数据库原始值校验，SQL NULL 时预期为 0 |
| `KingbaseMysqlTableMetadataParserTest.testApplyColumnInfoKeepEnumLiteralCase` | [MysqlTableMetadataParserUnitTest](https://github.com/hs-web/hsweb-easy-orm/blob/96b61b0b8e2f2cd9fc61f9fcc09f73c26ccd19db/hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/mysql/MysqlTableMetadataParserUnitTest.java) | 保留枚举字面量大小写、VARCHAR、String 断言 |
| `KingbaseMysqlDialectTest.testMysqlEnumDataType/testMysqlNumericDataTypeCaseInsensitive` | [DialectTest](https://github.com/hs-web/hsweb-easy-orm/blob/96b61b0b8e2f2cd9fc61f9fcc09f73c26ccd19db/hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/metadata/dialect/DialectTest.java) | 预期使用官方测试中的字面量、JDBC 常量及精度常量 |

`KingbaseMysqlOfficialTestDatabase` 只提供连接、独立临时 Schema、同步执行桥接和资源清理。
同步套件通过现有 `ReactiveSyncSqlExecutor` 运行，覆盖同步 ORM API，不代表 Kingbase JDBC 驱动已测。

字段属性别名沿用原生 MySQL 解析器的处理，保留字段名大小写，不再额外转换为小写。
`KingbaseMysqlTableMetadataParserTest` 对照原生解析器验证别名和混合大小写字段的查找行为。

`KingbaseMysqlR2dbcIntegrationTests` 的 50 项类型对照和解析器别名单元测试属于补充测试，
不宣称它们是官方测试。其固定 JDBC/Java 预期来自 MySQL 8.4.11 实库对照，
不会在测试运行时调用 MySQL 或 Kingbase 方言计算预期。官方未覆盖的类型也不得标成官方预期。

原生 MySQL 官方测试仍保留 INT 精度 10 的断言，不修改上游测试文件。
Kingbase 适配测试按已确认的规则直接查询 `information_schema.columns.numeric_precision`：
有值时保留原值，INT 精度为 SQL NULL 且字段长度为 0 时预期为 0，不强行补成 10。
该预期取自独立的原始 SQL 查询，不读取 ORM 结果生成预期。
`shouldPreserveReportedIntegerPrecisionAndDefaultNullToZero` 使用固定输入 `10` 和 `NULL`、
固定预期 `10` 和 `0`，分别验证原生 MySQL 与 Kingbase 解析器的行为。

运行 Kingbase 全部相关测试（密码可通过环境变量提供）：

```bash
export KINGBASE_MYSQL_R2DBC_USERNAME=system
export KINGBASE_MYSQL_R2DBC_PASSWORD='<password>'
mvn -o -Pkingbase-r2dbc -pl hsweb-easy-orm-rdb -am \
  '-Dtest=KingbaseMysql*,DialectTest' \
  -Dsurefire.failIfNoSpecifiedTests=false \
  -Dkingbase.mysql.r2dbc.integration=true \
  '-Dkingbase.mysql.r2dbc.url=r2dbc:kingbase://127.0.0.1:54321/test?mode=mysql' test
```

原生 MySQL 基线入口为 `Mysql8BasicTest,MysqlReactiveTests,MysqlTableMetaParserTest,MysqlTableMetadataParserUnitTest`。
本次验证用 `-Dtest.mysql.version=8` 统一使用 MySQL 8.4.11 镜像；
`MysqlReactiveTests` 和 `MysqlTableMetaParserTest` 的实库连接原本使用 MySQL 5.7 Provider，
此参数只切换镜像版本，不改测试断言。

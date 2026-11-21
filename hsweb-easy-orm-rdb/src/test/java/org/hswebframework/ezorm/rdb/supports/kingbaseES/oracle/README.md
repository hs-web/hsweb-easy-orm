# Kingbase Oracle 测试依据

“官方”指 hs-web/hsweb-easy-orm 上游 `4.2`，固定核对提交：
`96b61b0b8e2f2cd9fc61f9fcc09f73c26ccd19db`。
本仓库的原生 `OracleBasicTest`、`OracleReactiveTests`、`OracleTableMetaParserTest`、
`BasicCommonTests`、`BasicReactiveTests` 与该提交逐字一致，本轮没有修改这些上游文件。

| Kingbase 入口 | 官方来源 | 对齐方式 |
|---|---|---|
| `KingbaseOracleOfficialSyncTests` | [OracleBasicTest](https://github.com/hs-web/hsweb-easy-orm/blob/96b61b0b8e2f2cd9fc61f9fcc09f73c26ccd19db/hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/oracle/OracleBasicTest.java) → `BasicCommonTests` | 直接继承全部 11 个测试方法，不覆盖测试或断言 |
| `KingbaseOracleOfficialReactiveTests` | [OracleReactiveTests](https://github.com/hs-web/hsweb-easy-orm/blob/96b61b0b8e2f2cd9fc61f9fcc09f73c26ccd19db/hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/oracle/OracleReactiveTests.java) → `BasicReactiveTests` | 直接继承全部 13 个测试方法，不覆盖测试或断言 |
| `KingbaseOracleOfficialTableMetadataTests` | [OracleTableMetaParserTest](https://github.com/hs-web/hsweb-easy-orm/blob/96b61b0b8e2f2cd9fc61f9fcc09f73c26ccd19db/hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/oracle/OracleTableMetaParserTest.java) | 移植全部 3 个测试，18 条有效 Assert 原文保留，适配连接、Schema、执行器、目标方言；Record 写入键名使用金仓实际解析的属性名 |
| `KingbaseOracleOfficialIndexMetadataTests` | [OracleIndexMetadataParserTest](https://github.com/hs-web/hsweb-easy-orm/blob/96b61b0b8e2f2cd9fc61f9fcc09f73c26ccd19db/hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/oracle/OracleIndexMetadataParserTest.java) | 原测试流程及断言保留，覆盖 4 个索引和同步/响应式解析 |
| `KingbaseOracleOfficialBatchUpsertTests` | [OracleBatchUpsertIntegrationTest](https://github.com/hs-web/hsweb-easy-orm/blob/96b61b0b8e2f2cd9fc61f9fcc09f73c26ccd19db/hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/oracle/OracleBatchUpsertIntegrationTest.java) → `AbstractBatchUpsertIntegrationTest` | 直接继承全部 4 个实库测试，不覆盖测试或断言 |
| `KingbaseOracleTableMetadataParserTest.shouldParseOfficialOracleColumnMetadata` | 官方 `OracleTableMetaParserTest.testParse` | 以固定记录验证官方 VARCHAR2 / NUMBER 字段断言 |
| `KingbaseOracleDialectTest.shouldMatchOfficialOracleNumberAndVarchar2Contracts` | 官方 `OracleTableMetaParserTest.testParse` | 使用官方固定类型描述、JDBC 类型和 Java 类型 |

官方元数据合同包括：`varchar2(32 char)`、`varchar2(128 char)`、
`number(10,0)`、NUMBER 精度 10 / 小数位 0，以及 VARCHAR/String、NUMERIC/BigDecimal。
Kingbase 回读的别名不能用于改写这些官方预期。

原生 `OracleBatchUpsertOperatorTest` 的 9 个无数据库单元测试也可独立运行，
验证 Kingbase Oracle 复用的 `OracleBatchUpsertOperator`；它们不代表 Kingbase 实库验证。

`KingbaseOfficialTestDatabase` 只提供隔离 Schema、连接和清理；MySQL 与 Oracle 共用该测试设施。
同步 ORM 测试通过现有 `ReactiveSyncSqlExecutor` 桥接 R2DBC，
不代表 Kingbase JDBC 驱动已验证；上游 Oracle 响应式套件原本使用 JDBC 响应式执行器。

已有 16 项类型对照、目录别名、未知类型回退、存储过程等属于兼容性补充测试，
不能代替官方对齐结论。其预期使用固定常量，不在测试运行时调用 Oracle 或 Kingbase
方言生成预期。官方没有逐项断言的类型，不宣称已获官方认证或完成原生 Oracle 实库对照。
本环境使用 Kingbase Oracle 兼容实例运行，未运行原生 Oracle 数据库基线。

Record 写入和更新使用与元数据属性名一致的键名，不提供大小写自动补齐。
元数据名称查询按原生 Oracle 的大写归一化语义匹配 Schema、表和索引，返回的物理名称保持数据库原值。
函数索引从金仓目录读取表达式，保留复合索引中的表达式位置和字段升降序；
`KingbaseOracleMetadataCompatibilityTests` 补充覆盖同步、响应式、按名称查询及大小写名称查询。
官方断言失败时保留失败并报告，不按 Kingbase 当前返回值降低预期。

```bash
export KINGBASE_ORACLE_R2DBC_USERNAME=system
export KINGBASE_ORACLE_R2DBC_PASSWORD='<password>'
mvn -o -Pkingbase-r2dbc -pl hsweb-easy-orm-rdb -am \
  '-Dtest=KingbaseOracle*,DialectTest' \
  -Dsurefire.failIfNoSpecifiedTests=false \
  -Dkingbase.oracle.r2dbc.integration=true \
  '-Dkingbase.oracle.r2dbc.url=r2dbc:kingbase://127.0.0.1:54322/test?mode=oracle' test
```

只验证官方原始流程时，可将测试选择改为 `-Dtest=KingbaseOracleOfficial*`；
此入口覆盖同步、响应式、表/索引元数据及批量 upsert 的 32 项测试。

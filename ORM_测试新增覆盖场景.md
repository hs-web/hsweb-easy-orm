# ORM 测试新增覆盖场景

核对日期：2026-10-26。

## 核对范围与判定方式

本文记录当前 Kingbase MySQL、Kingbase Oracle 测试相对于官方 4.2.1 MySQL、Oracle 数据库测试增加的场景，以及对已有场景增加的专项断言。对比范围包含 `BasicCommonTests`、`BasicReactiveTests` 和各自的表、索引元数据解析测试。

“新增覆盖”限定于上述数据库测试的对比，不表示整个官方 ORM 仓库完全没有相关通用单元测试。例如官方另有编解码测试，而本文关注通过 Kingbase 驱动执行的数据库往返。本文以源码分析为主；字段属性回读、索引读取、表枚举和反向解析、注解实体、DDL 类型集合、结构变更、实体关联、分批插入、批次内主键合并和保存语义均有专项测试，不据此声明全部 ORM 测试已经通过，实际执行记录见本地缺失场景文档。

## 1. 连接与方言

| 新增或增强场景 | 适用模式 | 已有测试和断言 | 源码位置 |
|---|---|---|---|
| Kingbase 驱动与实际兼容模式确认 | 两个模式 | `shouldCreateConnectionAndUseMysqlCompatibilityMode()` / `shouldCreateConnectionAndUseOracleCompatibilityMode()` 验证工厂元数据名称 `KingbaseES`、实际连接、`show database_mode` 和绑定参数查询；共用实库测试初始化也确认实际模式 | [MySQL 连接测试][MC]、[Oracle 连接测试][OC]；[共用实库基类][K] 的 `setUp()` |
| 连接拒绝异常 | 两个模式 | `shouldFailWhenConnectionPortIsNotListening()` 连接本地未监听端口，断言 `R2dbcNonTransientResourceException` 和底层 `ConnectException`；输出 SQLState、错误码及原因。SQLState 和错误码仅打印，没有断言固定值 | [MC]、[OC] |
| Kingbase 方言标识和引用规则 | 两个模式 | 断言 `kingbase-mysql` / `kingbase-oracle` 标识、方言名称；MySQL 使用反引号，Oracle 使用双引号并转大写 | [MySQL 方言测试][MD]、[Oracle 方言测试][OD] |
| Oracle 模式真实 R2DBC 路径 | Oracle 模式 | 共用实库测试经 `r2dbc:kingbase` 建立连接，执行 DDL、DML、查询等；原生 `OracleReactiveTests` 实际采用 JDBC 包装执行器 | [K]；对比 [Oracle 原生响应式测试][OR] |

## 2. 元数据读取

| 新增或增强场景 | 适用模式 | 已有测试和断言 | 源码位置 |
|---|---|---|---|
| 混合大小写字段查找 | MySQL 模式 | `shouldSupportCaseInsensitiveLookupForMixedCaseColumns()` 解析模拟字段 `MixedName`，分别用小写、大写和混合大小写查找，断言返回同一字段对象 | [MySQL 字段解析测试][MP] |
| 小写字段通过大写名称查找 | Oracle 模式 | `shouldPreserveMetadataAndSupportUppercaseColumnLookup()` 验证模拟字段的名称、长度、非空属性，以及大写查找返回同一对象 | [Oracle 字段解析测试][OP] |
| DDL 新增字段后的实库重新读取 | 两个模式 | `shouldCreateDdlAndReadCaseInsensitiveMetadata()` 新增字段后创建全新的 Schema 和数据库元数据实例，重新读取并断言字段存在、`id`/`ID` 均可查到、索引非空；Oracle 分支另断言字符长度和类型名称 | [K] |
| 按索引名称回读 | 两个模式 | `shouldCreateDdlAndReadCaseInsensitiveMetadata()` 通过 `parseByNameReactive(indexName)` 断言索引名称及所属表；原生索引测试主要按表读取 | [K] |
| 唯一及复合索引的属性断言 | 两个模式 | `shouldReadUniqueAndCompositeIndexes()` 读取主键、普通、唯一、复合索引共 4 个，逐一验证唯一性、主键标识、所属表、字段数量和位置、升降序；唯一索引字段为 `position_no asc`，复合索引为 `name asc, nullable_text desc`。四类索引与官方场景对应，字段顺序、唯一性等严格断言属于增强 | [K] |
| 表枚举及两表批量反向解析的明确断言 | 两个模式 | `shouldEnumerateTablesAndReverseEngineerMetadata()` 在独立临时 Schema 中创建两张带引号的小写表，断言枚举结果、解析表数量及名称、实际名称、字段数量和长度。官方 Oracle 有对应流程，测试中部分结果仅打印；此处增加明确断言并在 MySQL 模式执行对应场景 | [K] |
| 批量解析后主键索引与主键列映射 | 两个模式 | 同一方法逐表断言主键索引的唯一性、主键标识、所属表及 id 字段，并断言解析出的 id 列本身也标记为主键和非空，验证批量解析时索引信息正确关联到表及字段 | [K] |
| 反向解析后仓储操作的结果断言 | 两个模式 | 将批量解析结果注册到新的 Schema，执行保存、按主键查询、修改、分页和删除；断言影响行数、实际字段值及删除后查询为空。对应官方 Oracle 的解析后仓储流程，增加结果验证，避免仅证明 SQL 执行未报错 | [K] |
| 查找不存在的必需字段 | MySQL 模式 | `shouldThrowWhenRequiredColumnDoesNotExist()` 解析字段后调用 `getColumnNow("missing_column")`，断言 `IllegalStateException`，消息包含字段名称和所属表 | [MP] |

## 3. 数据类型映射

| 新增或增强场景 | 适用模式 | 已有测试和断言 | 源码位置 |
|---|---|---|---|
| KES 字符类型别名识别 | 两个模式 | 识别 `character varying`，断言 JDBC 类型为 `VARCHAR`、Java 类型为 `String`；MySQL 生成 `varchar(64)`，Oracle 另识别 `varchar2` 并生成 `varchar2(64 char)` | [MD]、[OD] |
| 定点数字段精度和小数位的实库回读 | 两个模式 | `shouldReadColumnAttributesForCompatibilityMode()` 在原生 `id/name/age` 场景之外增加 `amount`：MySQL 建表使用 `decimal(12,2)`，Oracle 使用 `number(12,2)`；严格断言回读的 JDBC `NUMERIC`、Java `BigDecimal`、精度 12、小数位 2 和可空属性，并打印数据库原始属性与 ORM 解析结果 | [K] |
| 注解实体逐字段回读与更新断言 | 两个模式 | `shouldRoundTripAnnotatedEntityTypes()` 复用官方 BasicTestEntity，严格比较字符串、Byte、Long、Float、Double、BigDecimal、Boolean、Date、枚举、枚举数组和 JSON 列表；插入、更新、save 后分别回读。对应官方已有类型场景，额外覆盖 Boolean true→false、超出 int 范围的 Long、中文 JSON，以及官方 equals 排除的 Date 和 Address 的独立断言 | [K] |
| MySQL 六位小数秒日期字符串回读 | MySQL 模式 | 新实体实库测试触发 datetime(6) 返回字符串的情况；`shouldDecodeFractionalTimestampUsingColumnCodec()` 进一步验证非零小数秒 `.123456`、原有日期格式、Date 对象、编码行为及 String 类型日期字段不被改写。适配修复限定在 KES MySQL Schema 的 Date 及其子类字段 | [MySQL Schema 测试][MS]；[MySQL Schema 适配][MPROD] |
| Oracle LOB 与 CHAR 底层类型别名 | Oracle 模式 | 完整 DDL 实库测试发现 text、bytea、bpchar 的识别缺口，已补充 LONGVARCHAR/String、LONGVARBINARY/byte[]、CHAR/String 映射；`shouldMapLobAndCharCatalogAliases()` 验证类型及 CHAR 长度生成，实库测试逐字段断言 JDBC 类型，避免以 OTHER 通过 | [Oracle 方言测试][OD]；[Oracle 方言适配][OPROD] |
| 非法类型长度参数 | MySQL 模式 | `shouldRejectInvalidTypeLength()` 解析 `character varying(abc)`，断言 `NumberFormatException`，消息包含 `abc` | [MD] |
| 时间和空值的精确实库回读 | 两个模式 | `shouldExecuteCrudPagingBatchNullAndTimeThroughEasyOrm()` 写入固定 `LocalDateTime` 和 SQL NULL，更新其他字段后回读，断言时间值相等、空值仍为空。官方响应式分页也插入空值，但没有相同的时间和空值回读断言 | [K] |
| CLOB 与 JSON-CLOB 实库往返 | 两个模式 | `shouldRoundTripClobAndJsonClobThroughEasyOrm()` 验证编码结果为 `ClobValue`，写入并读取，断言文本和 JSON 中的模式、数值字段 | [K] |

## 4. DDL/DML生成

| 新增或增强场景 | 适用模式 | 已有测试和断言 | 源码位置 |
|---|---|---|---|
| 普通 CREATE INDEX SQL 精确断言 | MySQL 模式 | `shouldUseKingbaseMysqlDialectAndCreateIndexes()` 断言完整普通建索引 SQL；预期 SQL 不包含 `IF NOT EXISTS` | [MySQL Schema 测试][MS] |
| 完整 DDL 类型集合及回读断言 | 两个模式 | `shouldCreateTableWithAllOfficialDdlTypes()` 对应官方 testDDL 的建表类型，按模式断言生成类型、执行建表并重新加载元数据，严格检查 13 个字段的 JDBC 类型、关键长度、数值精度、主键及索引。将官方重复命名的 datetime/DATE 列拆开，保证两类都建列；属于对官方建表场景的对应覆盖及断言增强，不包含完整改表流程 | [K] |
| 结构变更后的元数据和数据保留 | 两个模式 | `shouldAlterColumnsAndPreserveDefaultsAndData()` 对应官方字段扩容、新增和删除场景；每阶段从实库重新读取元数据，断言 name 长度 128、test/age 新增及 test 删除，并验证主键、索引及变更前后记录保留。写入 100 字符验证扩容实际生效，增强了官方仅执行 DDL 的验证 | [K] |
| 数据库默认值的实际执行 | 两个模式 | 同一方法在建表时定义字符串默认值 '1'，新增 age 时定义数值默认值 0；通过原始 SQL 省略相应字段写入，验证已有行及新增行的结果，避免仅验证 ORM 在客户端填充默认值 | [K] |
| 实体流、分批及自动主键的实库结果 | 两个模式 | `shouldInsertEntityStreamsInBatches()` 对应官方 10 条实体流、100 条分批插入及未指定主键的 100 条批量插入，额外回读首尾数据、全部自动生成 ID/名称映射，断言 UUID 回填、唯一性和累计 210 条，增强官方主要校验影响行数的场景 | [K] |
| 批次合并后的字段值和扩展字段 | 两个模式 | `shouldMergeDuplicateEntityPrimaryKeysWithinBatch()` 对应官方主键合并和去重，额外断言后写入非空值生效、旧非空 balance/doubleVal 保留、ext_name 正确回读；分别检查 insertBatch/save 的六条输入实际合并为三行及最终值 | [K] |
| 保存行为的实际数据和空值保留 | 两个模式 | `shouldPreserveEntitySaveSemantics()` 对应官方新增、更新、混合保存、空值保留场景，断言未调用 save 前对象修改不落库、空枚举不覆盖原值、混合批次空 Double 保留旧值以及数据库记录数。遵循 SaveResult 分项不保证准确的契约，严格验证 total 和实际数据 | [K] |
| 已有索引的重复结构同步 | 两个模式 | `shouldAllowRepeatedIndexCreationThroughDdl()` 对已有索引再次声明相同结构，要求同步完成，并回读名称、所属表及单列 `name` | [K] |
| 索引字段变更后重建 | 两个模式 | `shouldRebuildIndexWhenColumnsChange()` 使用新 Schema 从实库获取原结构，将同名索引字段由 `name` 改为 `position_no`，回读并断言新字段 | [K] |
| Oracle 兼容建表 SQL 精确断言 | Oracle 模式 | `shouldRenderOracleCompatibleCreateTableSql()` 断言 `number(20,0)`、`varchar2(64 char)`、主键、非空约束及完整建表 SQL | [Oracle Schema 测试][OS] |
| 注释语句批次完整性 | Oracle 模式 | `shouldRetainAllCommentsAfterCreateTableStatement()` 断言建表请求中的两条附加 SQL 依次为字段注释和表注释；属于生成测试，没有断言数据库注释回读内容 | [OS] |
| 建表字段缺少类型 | Oracle 模式 | `shouldRejectCreateTableWhenColumnTypeIsMissing()` 断言 `UnsupportedOperationException`，消息以 `unknown column type : ` 开头且包含字段名 | [OS] |
| 插入字段全部无效 | MySQL 模式 | `shouldRejectInsertWhenNoRequestedColumnExists()` 仅指定不存在的插入字段，在 `getSql()` 阶段断言 `IllegalArgumentException`，消息为 `No operable columns` | [MS] |

重复索引同步测试没有通过计数独立证明“只执行了几条 SQL”，也没有断言完全不访问数据库；它证明相同结构可以再次同步且回读结构正确。

## 5. 查询分页

framework 树形服务实库回归发现 Oracle 模式 empty/notempty/nempty 的空字符串条件问题：祖先查询中的 PATH != '' 将结果过滤为空。已仅在 KingbaseOracleSchemaMetadata 注册 NullTermFragmentBuilder，分别生成 IS NULL/IS NOT NULL；[OS] 的 shouldUseNullPredicatesForOracleEmptyStringConditions 覆盖三个名称并保护 MySQL 原有 != '' 行为。Oracle Schema 四项单元测试通过；framework 两个模式各八项树形实库测试通过，其中包括祖先查询，未修改公共构造器或原生 Oracle 实现。

| 新增或增强场景 | 适用模式 | 已有测试和断言 | 源码位置 |
|---|---|---|---|
| 缺失分组字段的查询生成异常 | MySQL 模式 | `shouldRejectPagingQueryWhenGroupByColumnDoesNotExist()` 对不存在的字段分组并设置分页，在 `getSql()` 阶段断言 `IllegalStateException`，消息包含缺失字段和表名。该测试验证字段错误，不是非法分页参数 | [MS] |
| 分组、空值条件、表达式更新和锁定查询的组合验证 | 两个模式 | `shouldSupportGroupingNativeUpdateNullPredicateAndForUpdate()` 断言分组数量为 2、显式 `IS NULL OR ...` 的计数为 2，并在表达式更新后使用 `FOR UPDATE` 查询确认数值为 2。原生也有分组、表达式更新和 `FOR UPDATE` 测试，这里属于断言增强及组合场景；没有验证锁竞争 | [K] |
| 模式特定的枚举 ANY 条件 | 两个模式 | `shouldQueryEnumMaskUsingModeSpecificBitAnd()` 手动注册枚举掩码编解码器，检查对应特性存在，并断言 `in$any` 命中、不命中及 `nin$any` 结果。原生已有 ANY 功能测试，此处主要增加 KES 模式适配路径的验证 | [K] |
| 注解枚举的完整条件和回读 | 两个模式 | `shouldMatchAnnotatedEnumMasksWithInAndAnyConditions()` 对齐官方普通 in 和 ANY 条件，并用官方实体注解自动注册编解码器；额外断言完整枚举数组回读、ANY 不命中和 NOT ANY 不命中，避免只验证正向结果 | [K] |
| 否定条件的 NULL 对照 | 两个模式 | `shouldIncludeNullValuesInNegatedConditions()` 对齐官方四种否定条件计数 2，再改变比较值排除非空记录，逐一断言只返回 NULL 记录 ID，验证 NULL 保留与非空过滤同时有效 | [K] |
| 比较条件中的表达式及零行对照 | 两个模式 | `shouldUseNativeExpressionsInUpdateAndComparisonConditions()` 对齐官方 SET 和 lte 中的 NativeSql，额外回读目标状态 1→2、确认对照不变，并验证不成立的表达式条件影响 0 行且数据不变 | [K] |
| 五条记录的指定值和后续字段排序 | 两个模式 | `shouldOrderBySpecifiedValuesThenMultipleColumns()` 对齐官方 desc(value) 及 createTime/state 排序，增加 asc(value) 和未指定记录；逐项比较五条 ID，验证指定值 asc 排前、desc 排后及时间/状态排序，不依赖插入顺序或普通 ID 顺序 | [K] |
| 关联嵌套条件的有效性及投影排除 | 两个模式 | `shouldExecuteAnnotatedRepositoryAndAssociationOperations()` 对应官方实体关联及嵌套查询，使用 id=missing OR address.name=test_address，强制由关联条件选中记录；断言排序分页结果和排除 address.* 后关联对象为空。增加对照记录，避免只用单条数据证明条件可执行 | [K] |
| 关联条件更新/删除及不匹配对照 | 两个模式 | 同一方法验证关联条件匹配时更新/删除各影响 1 行，条件不匹配时各返回 0；回读确认目标状态、目标删除结果和对照实体及其关联地址保留。对应官方已有更新/删除场景，并增加范围控制的结果断言 | [K] |
| 指定值排序的多行结果校验 | 两个模式 | `shouldPreserveEntitySaveSemantics()` 执行官方保存测试中的指定值及多字段排序，并用两条记录断言 save-existing 排在 save-new 前；此顺序不同于普通 ID 降序，可验证 value 参数实际生效，增强官方单行查询的排序约束 | [K] |
| 连续十页的完整序列及分页边界 | 两个模式 | `shouldReadTenConsecutivePagesWithoutDuplicatesOrGaps()` 对应官方每页求和场景，额外按 ID 升序逐页断言完整序列、所有 id2 空值、全局 100 条唯一 ID 和总和 5050，并验证末页后页码 10 返回空结果；显式排序消除物理存储顺序的不确定性 | [K] |
| 官方 getter 条件链及 AND/OR 括号优先级 | 两个模式 | 增强已有 `shouldExecuteAnnotatedRepositoryAndAssociationOperations()`，复现官方 getter 条件、字段排除、排序分页及实体相等校验；另验证根 ID 限定下嵌套 OR 不匹配应返回空结果，即使对照记录满足 OR 中的关联分支也不能被选中 | [K] |

## 6. 异常处理与事务

| 新增或增强场景 | 适用模式 | 已有测试和断言 | 源码位置 |
|---|---|---|---|
| KES R2DBC 异常转换和 cause 保留 | 两个模式 | `shouldTranslateKingbaseDuplicateKeySqlState()` 构造 SQLState 为 `23505`、消息为 `duplicate key` 的异常，断言转换成 ORM `DuplicateKeyException` 且保留原异常为 cause。两个条件同时成立，不能据此声称已独立覆盖 SQLState 和消息识别分支 | [MySQL 异常转换测试][ME]、[Oracle 异常转换测试][OE] |
| Oracle 模式重复插入的实库异常转换 | Oracle 模式 | `shouldTranslateDuplicateKeyDuringReactiveInsert()` 两次插入相同主键，断言 ORM `DuplicateKeyException`。原生 Oracle 对应测试套件没有同类专用用例；原生 MySQL 已有 `testExceptionTrans()`，因此 MySQL 模式属于对应覆盖而非独有功能 | [K] |
| 保存点回滚并提交 | 两个模式 | `shouldRollbackToSavepointAndCommitTransaction()` 插入第一条、建立保存点、插入第二条、回滚到保存点再提交，断言最终只有一条记录 | [K] |
| 整体事务回滚 | 两个模式 | `shouldRollbackTransaction()` 事务内插入后回滚，断言记录数为 0 | [K] |

事务测试直接通过 R2DBC `Connection` 操作，证明 Kingbase 驱动及数据库的该条事务路径；它们不证明 Spring 事务集成或其他 ORM 事务封装已覆盖，后者属于其他层面的测试。

## 7. 使用本文时的边界

- 这些新增测试不能抵消官方功能场景中的缺口，缺失清单见 [ORM_测试缺失场景.md](ORM_测试缺失场景.md)。
- 同一个共用测试方法由两个模式分别继承执行，表示两条模式路径，不能据此认定所有模式特定分支都已验证。
- 注解实体测试使用官方已有的 `NestedEntityResultWrapper`。普通 `EntityResultWrapper` 存在枚举数组二次解码问题，本次未改公共实现，该路径不属于本次通过结论。
- 实库测试需要显式启用，并提供相应驱动和连接条件；连接拒绝测试无需数据库，但需要 Kingbase 驱动，否则会跳过。
- 当前目录下的这两份文档仅用于本地查阅，创建后未执行 `git add`、commit 或 push，也未修改忽略规则。

[K]: hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/kingbase/AbstractKingbaseR2dbcIntegrationTests.java
[MC]: hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/kingbase/mysql/KingbaseMysqlR2dbcConnectionFactoryTests.java
[OC]: hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/kingbase/oracle/KingbaseOracleR2dbcConnectionFactoryTests.java
[MD]: hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/kingbase/mysql/KingbaseMysqlDialectTest.java
[OD]: hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/kingbase/oracle/KingbaseOracleDialectTest.java
[MP]: hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/kingbase/mysql/KingbaseMysqlTableMetadataParserTest.java
[OP]: hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/kingbase/oracle/KingbaseOracleTableMetadataParserTest.java
[MS]: hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/kingbase/mysql/KingbaseMysqlSchemaMetadataTest.java
[OS]: hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/kingbase/oracle/KingbaseOracleSchemaMetadataTest.java
[ME]: hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/kingbase/mysql/KingbaseMysqlR2DBCExceptionTranslationTest.java
[OE]: hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/kingbase/oracle/KingbaseOracleR2DBCExceptionTranslationTest.java
[OR]: hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/oracle/OracleReactiveTests.java
[MPROD]: hsweb-easy-orm-rdb/src/main/java/org/hswebframework/ezorm/rdb/supports/kingbase/mysql/KingbaseMysqlSchemaMetadata.java
[OPROD]: hsweb-easy-orm-rdb/src/main/java/org/hswebframework/ezorm/rdb/supports/kingbase/oracle/KingbaseOracleDialect.java

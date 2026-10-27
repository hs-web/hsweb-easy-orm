# ORM 测试缺失场景

核对日期：2026-10-26。

2026-10-27 完整执行更新：已执行根项目 Maven test，并补跑旧 Surefire 2.12.4 默认未发现的 `*Tests` 类，两个 Kingbase 实库开关均开启。合并本轮报告后总计 441 项：222 通过、0 断言失败、217 错误、2 跳过；其中 Kingbase 命名测试 73 项全部通过、无错误或跳过。原生数据库相关错误来自 Testcontainers 1.17.1 使用 Docker API 1.32，而本机 Docker 29.1.3 最低支持 1.44，及容器未启动后的连接失败；不能据此判断原生功能或 Kingbase 适配有缺陷。未修复公共实体解码，也未修改业务实现。完整统计和命令见相邻 framework 项目的 [全量测试报告](../hsweb-framework/ORM_framework_全量测试结果_2026-10-27.md)。

2026-10-27 依赖更新：只保留一个 `kingbase-r2dbc` 驱动 Profile，并默认自动激活，使用 `com.github.zz97523:r2dbc-kingbase:main-SNAPSHOT`，通过 JitPack 获取。旧 PostgreSQL 驱动回退配置已移除，不再需要显式指定 `-Pkingbase-r2dbc`；原有 `release` Profile 同时启用时也保留 Kingbase 配置。本次解析到提交 `be49b83f35063d0266ca335333a0f518b112cc23`。与适配相关的非实库测试执行 21 项，其中 19 项通过、2 项真实连接用例按开关跳过，0 失败、0 错误；两种模式的无监听端口连接拒绝测试均实际通过。本轮未连接实库，下文此前的实库结果不作为新驱动实库回归结论。默认配置复验日志：`/tmp/hsweb-orm-default-kingbase-tests.log`。

## 核对范围与结论

对比当前 Kingbase MySQL、Kingbase Oracle 测试与官方 4.2.1 基线中 MySQL、Oracle 的数据库测试，包括其继承的 `BasicCommonTests`、`BasicReactiveTests` 和表、索引元数据解析测试。相关原生测试相对该基线没有修改。

当前两个 Kingbase 模式已逐项补齐本清单核对出的官方 R2DBC 功能对比缺口，各阶段实际执行结果见下文。这是针对所列官方数据库测试操作和断言的功能对比，不代表整个官方仓库测试、全部包装器路径或代码分支均已覆盖，也没有执行全量测试。缺少测试证据不等于功能不支持。

本次已补齐 `shouldReadColumnAttributesForCompatibilityMode()`，两个模式实库各执行 1 项，均通过，失败、错误和跳过数均为 0。它覆盖原生示例的 `id`、`name`、`age` 属性回读，额外验证 `amount` 的精度 12、小数位 2；本次没有执行全部 ORM 测试。

随后已补齐 `shouldReadUniqueAndCompositeIndexes()`，两个模式实库各执行 1 项，均通过，失败、错误和跳过数均为 0。验证总计 4 个索引：主键、普通、唯一、复合索引，并严格校验唯一性、主键标识、所属表、字段顺序及升降序。唯一和复合索引读取、主键索引标识不再列为待补缺口。

现已补齐 `shouldEnumerateTablesAndReverseEngineerMetadata()`，两个模式实库各执行 1 项，均通过，失败、错误和跳过数均为 0。通过 R2DBC 的 `parseAllTableNameReactive()`、`parseAllReactive()` 对应官方 Oracle 的表枚举、`parseAll()` 场景：在独立临时 Schema 中枚举和解析两张表，验证小写实际表名、字段及主键索引和主键列标识，注册解析结果后执行保存、查询、修改、分页和删除。临时 Schema 在 finally 中清理。同步 JDBC 调用路径仍不纳入本次范围。

现已补齐 `shouldRoundTripAnnotatedEntityTypes()`，两个模式实库各执行 1 项，均通过，无失败、错误或跳过。复用官方 `BasicTestEntity`、`Address`、JPA 注解解析器，以及 `BasicCommonTests` 使用的 `NestedEntityResultWrapper`，通过 R2DBC 验证插入、实体回读、多类型更新、保存、删除和关联地址回读。逐项比较 String、Byte、Long、Float、Double、BigDecimal、Boolean、Date、枚举、枚举数组和 JSON 列表；其中 Boolean 覆盖 true 更新为 false。对应注解实体多类型回读的缺口已完成。

该测试发现 KES MySQL 的 datetime(6) 可能以带六位小数秒的字符串返回，原有日期解析失败。已仅在 `KingbaseMysqlSchemaMetadata` 中为 Date 及其子类字段补充字符串时间戳规整，委托原编解码器继续处理，未修改公共 DateTimeCodec。日期回归测试位于 `KingbaseMysqlSchemaMetadataTest.shouldDecodeFractionalTimestampUsingColumnCodec()`。

现已补齐 `shouldCreateTableWithAllOfficialDdlTypes()`，覆盖官方 `BasicCommonTests.testDDL()` 的建表类型集合。两个模式均按各自方言验证生成类型、执行 R2DBC 实库建表，使用新 Schema 元数据回读并严格断言 13 个字段的 JDBC 类型、关键长度、精度、主键和索引。官方将 datetime 和 DATE 都命名为 date_test，本测试拆成 datetime_test、date_test，确保两类都实际建列；其结构变更部分由下述独立测试覆盖。

实库回读发现 Oracle CLOB、BLOB、CHAR 的底层类型名为 text、bytea、bpchar，原来被识别为 OTHER。已在 `KingbaseOracleDialect` 中分别映射为 LONGVARCHAR/String、LONGVARBINARY/byte[]、CHAR/String，并用方言单元测试及本次实库类型断言验证。

现已补齐 `shouldAlterColumnsAndPreserveDefaultsAndData()`，两个模式实库均通过。对应官方 testDDL 的 name 长度 64→128、新增 test/age、age 默认值 0、删除 test；额外验证建表默认值、已有行回填默认值、原始 SQL 省略字段后的默认值、100 字符写入，以及变更后数据、主键和索引保留。每次变更均用新的 Schema 元数据从数据库回读。

现已补齐 `shouldExecuteAnnotatedRepositoryAndAssociationOperations()`，两个模式实库均通过。对应官方 testJoin/testRepositoryCurd 的实体 CRUD 和 Address 关联回读、嵌套 AND/OR 条件、排除 address.*、排序分页，以及关联条件下更新、删除；额外加入不匹配关联条件的对照记录，确认更新、删除返回 0 且不影响该记录。

现已补齐 `shouldInsertEntityStreamsInBatches()`，两个模式实库均通过：insert(Flux) 写入 10 条，insertBatch(Flux<List<实体>>) 分 10 批写入 100 条，再通过实体集合批量写入 100 条未指定主键的数据，验证 UUID 回填、唯一性及全部 ID/名称回读，累计 210 条。对应官方同步及响应式 testRepositoryInsertBach 的功能场景，执行路径限定为 R2DBC。

现已补齐 `shouldMergeDuplicateEntityPrimaryKeysWithinBatch()`，两个模式实库均通过：相同主键的两条不同值输入合并为一行，后写入的非空值生效，旧非空值和 ext_name 扩展字段保留；insertBatch 和 save 分别验证六条输入合并为三行、最终字段值及删除行数。对应官方 testInsertMerge/testInsertDuplicate，和已存在记录再次 insert 的重复键异常属于不同场景。

现已补齐 `shouldPreserveEntitySaveSemantics()`，两个模式实库均通过：save 新增、修改对象未保存前数据库保持旧值、save 更新、空枚举保存保留数据库原值、同批混合新增更新时空 Double 保留旧值，并验证总数及实际数据。对应官方 testRepositorySave/testReactiveRepositorySave/testSaveNull；只严格断言 SaveResult.total，added/updated 分项按官方契约不保证准确。另用两条记录验证指定值及多字段排序。

现已补齐 `shouldReadTenConsecutivePagesWithoutDuplicatesOrGaps()`，两个模式实库均通过。对应官方 testPager/testReactivePager：写入 100 条数值主键和空 id2，按 ID 升序连续读取页码 0–9、每页 10 条，逐页断言完整 ID 序列及官方求和公式，检查所有空值和全局无重复、无遗漏；额外验证页码 10 返回空结果。显式排序避免依赖物理存储顺序。

已有 `shouldExecuteAnnotatedRepositoryAndAssociationOperations()` 再次增强并在两个模式下通过：复现官方 getter 根条件 AND (根字段条件 OR 关联字段条件) 的查询链，执行字段排除、排序分页和实体相等校验；保留强制由关联 OR 分支命中的场景，并增加根条件与嵌套 OR 不匹配时返回空结果的断言，防止括号优先级错误纳入对照记录。关联更新、删除及不匹配返回 0 的流程一并回归。

建表类型按模式选择：MySQL 使用 `varchar`、`int`、`decimal(12,2)`；Oracle 使用 `varchar2`、`number(10,0)`、`number(12,2)`。KES MySQL 视图未提供 integer 精度，ORM 回读为 0；这不代表整数可表示的位数为 0。Oracle 的 number 回读类型描述为 numeric，精度和小数位单独校验。

两个模式均继承 [Kingbase 共用实库测试基类][K]，没有继承官方的 [同步测试基类][C] 或 [响应式测试基类][R]。

## 1. 相对官方测试缺少或覆盖不足的场景

### 1.1 连接与方言

已明确本次连接与方言只要求 R2DBC，不纳入同步 JDBC 补测。当前已有 R2DBC 连接、模式确认、方言及连接拒绝场景，本模块没有待补的官方对比缺口。

原生 [OracleReactiveTests][OR] 使用 `TestJdbcReactiveSqlExecutor`，实际通过 JDBC 执行；其 R2DBC 执行器代码被注释。Kingbase Oracle 使用真实 R2DBC 驱动，因此两者驱动路径不同，不能仅按“响应式测试”名称判断等价。

### 1.2 元数据读取

本次核对的官方元数据测试场景已具备对应 R2DBC 测试，本节暂无待补的官方对比缺口。

| 已完成场景 | 官方测试依据 | KES 对应测试 |
|---|---|---|
| 字段属性回读 | [MT]、[OT] 的 `testParse()` | `shouldReadColumnAttributesForCompatibilityMode()` |
| 普通、唯一、复合索引和主键标识 | [MI]、[OI] 的 `test()` | `shouldReadUniqueAndCompositeIndexes()` |
| 表名枚举与批量反向解析、带引号小写表名、解析后仓储操作 | [OT] 的 `testParseReactive()`、`testParseAll()` | `shouldEnumerateTablesAndReverseEngineerMetadata()` |

以上方法均位于 [K]，由两个兼容模式的测试入口分别执行。重复结构同步和索引字段变更也已有测试，不应将已完成场景重复列为未覆盖。

### 1.3 数据类型映射

官方注解实体多类型回读及 testDDL() 的建表类型集合已具备对应 R2DBC 测试，本节原列出的两个官方对比缺口均已完成。

DDL 测试覆盖 VARCHAR、NUMERIC、TIMESTAMP、INTEGER、BIGINT、CLOB、TINYINT、BLOB、CHAR、TIME、DATE；其中 VARCHAR 使用三个不同字段及长度，合计 13 个字段。Oracle 的 TIME 按官方方言生成为 timestamp，DATE 生成为 date、回读为 TIMESTAMP。此结论限定于 DDL 生成、执行及元数据类型回读，不等于所有类型的 DML 值往返均已验证。

注解实体多类型操作与回读已由 [K] 的 `shouldRoundTripAnnotatedEntityTypes()` 覆盖，对比依据为 [E]、[C] 的 `testRepositorySave()`、`testRepositoryCurd()` 和 [R] 的 `testRepositoryCurd()`。日期和关联地址采用独立断言，不依赖官方实体中排除了这两个字段的 equals 比较。此处不代表已覆盖全部数据库类型；CLOB/JSON 和枚举掩码也有独立实库测试。

本次使用官方已有的嵌套实体包装器。排查时发现普通 `EntityResultWrapper` 与查询层同时解码，会使非空枚举数组被二次解码为空；这是公共层路径的问题，本次未修改该公共实现，也不宣称该包装器路径已覆盖。自动生成主键、批量实体操作、空值更新规则已由上述专项测试验证。

### 1.4 DDL/DML生成

本节原列的结构变更、实体关联、分批插入、批次内重复主键合并和保存语义均已完成对应 R2DBC 功能测试，暂无待补的官方对比缺口。

| 已完成场景 | 官方测试依据 | KES 对应测试 |
|---|---|---|
| 结构变更及默认值 | [C] 的 `testDDL()`、`testCrud()` | `shouldAlterColumnsAndPreserveDefaultsAndData()` |
| 实体仓储及关联操作 | [C] 的 `testJoin()`、`testRepositoryCurd()`；[R] 的 `testRepositoryCurd()` | `shouldExecuteAnnotatedRepositoryAndAssociationOperations()`、`shouldRoundTripAnnotatedEntityTypes()` |
| 实体流分批插入及自动主键 | [C]、[R] 的 `testRepositoryInsertBach()` | `shouldInsertEntityStreamsInBatches()` |
| 批次内主键合并、去重及扩展字段 | [C] 的 `testInsertDuplicate()`；[R] 的 `testInsertMerge()`、`testInsertDuplicate()` | `shouldMergeDuplicateEntityPrimaryKeysWithinBatch()` |
| save 新增、更新、混合保存及空值保留 | [C] 的 `testRepositorySave()`；[R] 的 `testReactiveRepositorySave()`、`testSaveNull()` | `shouldPreserveEntitySaveSemantics()` |

以上方法均位于 [K]。共用注解实体准备方法按官方 BasicReactiveTests 增加 ext_name 字段并 reload 映射，以验证扩展字段合并；原有两个模式的多类型实体测试同时回归。

### 1.5 查询分页

后续 framework 树形服务测试发现 Oracle 空字符串条件导致祖先查询为空，已补充修复和回归：KingbaseOracleSchemaMetadata 将 empty/notempty/nempty 对应到 IS NULL/IS NOT NULL；KingbaseOracleSchemaMetadataTest.shouldUseNullPredicatesForOracleEmptyStringConditions 验证三种 SQL 并确认 MySQL 保持 != ''。Oracle Schema 四项单元测试、framework 两个模式各八项树形实库测试均通过。单元日志为 /tmp/hsweb-orm-oracle-empty-after.log，树形及必测回归日志为 /tmp/hsweb-framework-tree-required-final.log；仅修改 Kingbase 适配，不改公共或原生 Oracle 代码。

本节原列的三个缺口均已补齐，两个模式分别通过实库验证。以下方法均位于 [K]。

| 已完成场景 | 官方测试依据 | KES 对应测试及断言 |
|---|---|---|
| 普通枚举与 ANY 条件 | [C]、[R] 的 `testEnums()` | `shouldMatchAnnotatedEnumMasksWithInAndAnyConditions()`：写入 enabled+disabled，普通 in(enabled) 不匹配、完整组合匹配，并回读枚举数组；ANY disabled 命中、warn 不命中，NOT ANY warn 命中、enabled 不命中。复用官方注解解析器和实体，不手动替换编解码器。 |
| 否定条件包含空值 | [R] 的 `testNotOrNULL()` | `shouldIncludeNullValuesInNegatedConditions()`：准备 addressId 为 NULL 和 0 的两条实体，notLike/no、not/no、notIn/no、notBetween/1..3 均计数 2；再使非空记录不满足否定条件，四种查询都严格断言只返回 NULL 记录的 ID。 |
| 原生 SQL 表达式用于更新和比较条件 | [R] 的 `testUpdateByNative()` | `shouldUseNativeExpressionsInUpdateAndComparisonConditions()`：SET state=state+1、WHERE state<=state+1 更新 1 行，回读目标 1→2，对照仍为 3；改成 state<=state-1 后影响 0 行，目标和对照均不变。 |
| 指定值及多字段排序 | [R] 的 `testReactiveRepositorySave()` | `shouldOrderBySpecifiedValuesThenMultipleColumns()`：五条实体打乱插入顺序，按官方 desc(id).value(...)、createTime desc、state desc 逐项断言完整 ID 顺序；额外验证 asc(value)。指定值 asc 排前、desc 排后，未指定的三条记录按时间降序、同时间按状态降序。 |

原有 `shouldPreserveEntitySaveSemantics()` 的两条记录指定值排序断言继续保留；新增独立排序测试同时验证未指定记录及后续排序字段确实生效。

连续十页分页已由 `shouldReadTenConsecutivePagesWithoutDuplicatesOrGaps()` 覆盖；关联与嵌套条件已由增强后的 `shouldExecuteAnnotatedRepositoryAndAssociationOperations()` 覆盖，均不再列为缺口。

### 1.6 异常处理

原生 MySQL 的 `testExceptionTrans()` 验证重复插入转换为 ORM `DuplicateKeyException`。两个 Kingbase 模式均已有 `shouldTranslateDuplicateKeyDuringReactiveInsert()` 对应实库场景；这一点不属于缺失。

但现有 KES 异常转换单元测试同时提供消息 `duplicate key` 和 SQLState `23505`，无法独立证明每条识别分支都正确。需要补齐的适配专项分支见下一节；它们不应被描述成“官方已有、KES 缺失”的场景。

## 2. 官方数据库测试也未完整覆盖、但适配仍可补测的场景

本节是适配增强建议，与上节的官方等价覆盖缺口分开统计。

| 模块 | 当前证据不足的场景 | 适用模式 | 建议断言 |
|---|---|---|---|
| 元数据读取 | 表注释、字段注释从数据库系统目录正确回读 | 两个模式 | 从新的 Schema 读取后，注释内容与预期一致；仅生成或执行注释 SQL 不足以证明读取正确。 |
| 数据类型映射 | KES 新增类型别名的完整映射矩阵 | 两个模式 | 按各自方言中实际注册的类型，验证数据库类型识别、Java/JDBC 类型和生成 SQL；Boolean 的 true/false 实体往返已完成。 |
| 异常处理 | 错误码 1062、1022、SQLState 23505、消息关键字分别命中 | 两个模式 | 每次只让一个识别条件成立，断言转换结果及原始 cause；避免多个条件同时成立掩盖分支缺陷。 |
| 异常处理 | SQLState 23000 的转换策略 | Oracle 模式 | 为该分支建立单独用例；结合实际驱动错误验证适用范围，尤其区分唯一约束和其他完整性约束。 |
| 异常处理 | 非重复键错误、非 R2DBC 异常和空消息 | 两个模式 | 非目标错误保持原样，空消息不导致额外异常。 |
| 异常处理 | JDBC 异常转换器 | 两个模式 | 如保留 JDBC 支持声明，直接构造 `SQLException` 覆盖识别与透传，并补充对应驱动集成验证。 |
| 异常处理 | 负向测试在两个模式间不对称 | 两个模式 | MySQL 目前有非法类型长度、缺失字段查找、无有效插入字段和缺失分组字段测试；Oracle 有缺失字段类型的 DDL 测试。按实际公共契约补齐另一模式，不机械复制不适用的预期。 |
| 查询分页 | 非法页码和页大小的明确行为 | 两个模式 | 先确认当前 API 的处理逻辑，再验证其实际结果；现有“分组字段不存在”的异常测试不属于分页参数异常。合法页码超出末页返回空结果的场景已验证。 |

## 3. 建议补齐顺序

1. 本清单所列官方 R2DBC 功能对比缺口已逐项补齐，包括普通枚举匹配、否定条件含空值、比较条件中的原生表达式及指定值排序；这不等于官方全量套件或代码分支覆盖率为 100%。
2. 适配专项增强可继续补异常转换独立分支、非目标错误透传等，见第 2 节；这些不属于官方已有但当前缺失的场景。
3. 本次范围已确定为 R2DBC，同步 JDBC 套件不纳入补测；已完成的实体和批量操作不重复列为缺口。

## 4. 执行结果的判定

当前共用 Kingbase 实库测试通过 `kingbase.mysql.r2dbc.integration=true` 或 `kingbase.oracle.r2dbc.integration=true` 等配置显式启用，需具备驱动和数据库连接条件。未启用时会跳过，不能用普通 `mvn test` 成功替代两个模式实库场景的执行证据。

本次字段属性回读的日志：`/tmp/hsweb-orm-column-attributes-mysql.log`、`/tmp/hsweb-orm-column-attributes-oracle.log`。后续补测仍需分别记录两个模式实际执行的测试、跳过数和失败数。

本次唯一及复合索引读取的日志：`/tmp/hsweb-orm-index-read-mysql.log`、`/tmp/hsweb-orm-index-read-oracle.log`。

本次表枚举及批量反向解析的日志：`/tmp/hsweb-orm-reverse-engineering-mysql.log`、`/tmp/hsweb-orm-reverse-engineering-oracle.log`。

本次注解实体多类型操作的日志：`/tmp/hsweb-orm-annotated-types-mysql.log`、`/tmp/hsweb-orm-annotated-types-oracle.log`；日期相关单元测试日志：`/tmp/hsweb-orm-annotated-date-unit.log`。

注解实体阶段最终验证：两个模式的注解实体实库测试各 1 项、MySQL Schema 相关单元测试 4 项、原有 MySQL CRUD/分页/空值/时间实库回归 1 项，共 7 项通过，无失败、错误或跳过。原有流程回归日志为 `/tmp/hsweb-orm-annotated-types-mysql-regression.log`，没有运行全量测试。

完整 DDL 类型集合阶段日志：`/tmp/hsweb-orm-ddl-types-mysql.log`、`/tmp/hsweb-orm-ddl-types-oracle.log`；Oracle 方言单元测试日志：`/tmp/hsweb-orm-ddl-types-oracle-unit.log`。

完整 DDL 类型集合阶段最终验证：两个模式实库测试各 1 项，Oracle 方言单元测试 2 项，共 4 项通过，无失败、错误或跳过；本轮没有运行全量测试。

结构变更日志：`/tmp/hsweb-orm-alter-mysql.log`、`/tmp/hsweb-orm-alter-oracle.log`；实体关联日志：`/tmp/hsweb-orm-association-mysql.log`、`/tmp/hsweb-orm-association-oracle.log`。注解实体建表及映射准备代码提取为共用测试方法，并分别回归两个模式原有的多类型实体测试，日志为 `/tmp/hsweb-orm-association-mysql-types-regression.log`、`/tmp/hsweb-orm-association-oracle-types-regression.log`。

结构变更与实体关联阶段最终验证：两个模式的结构变更、实体关联、多类型实体回归各 1 项，共 6 项实库测试通过，无失败、错误或跳过；本轮改动为测试和覆盖清单，没有运行全量测试。

本次三项测试日志分别为 `/tmp/hsweb-orm-entity-batch-{mysql,oracle}.log`、`/tmp/hsweb-orm-entity-merge-{mysql,oracle}.log`、`/tmp/hsweb-orm-entity-save-{mysql,oracle}.log`；共用注解夹具调整后的多类型回归日志为 `/tmp/hsweb-orm-entity-bulk-types-regression-{mysql,oracle}.log`。

批量与保存语义阶段最终验证：两个模式各执行三项新增测试和一项原有多类型回归，共 8 项实库测试通过，无失败、错误或跳过；本轮仅修改测试及覆盖清单，没有运行全量测试。

连续分页与关联嵌套阶段最终验证：两个模式各执行连续十页分页、增强后的实体关联测试各 1 项，共 4 项实库测试通过，无失败、错误或跳过。日志为 `/tmp/hsweb-orm-continuous-paging-{mysql,oracle}.log`、`/tmp/hsweb-orm-association-nested-{mysql,oracle}.log`；本轮仅修改测试及覆盖清单，没有运行全量测试。

枚举、空值、表达式及排序阶段最终验证：两个模式各执行上述四项专项测试，共 8 项实库测试通过，失败、错误、跳过均为 0。日志及对应 XML 快照为 `/tmp/hsweb-orm-enum-conditions-{mysql,oracle}.{log,xml}`、`/tmp/hsweb-orm-negated-null-{mysql,oracle}.{log,xml}`、`/tmp/hsweb-orm-native-condition-{mysql,oracle}.{log,xml}`、`/tmp/hsweb-orm-specified-order-{mysql,oracle}.{log,xml}`。本轮仅新增测试并更新本地清单，没有修改生产代码，没有运行全量测试。

[C]: hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/BasicCommonTests.java
[R]: hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/BasicReactiveTests.java
[K]: hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/kingbase/AbstractKingbaseR2dbcIntegrationTests.java
[E]: hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/BasicTestEntity.java
[MT]: hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/mysql/MysqlTableMetaParserTest.java
[OT]: hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/oracle/OracleTableMetaParserTest.java
[MI]: hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/mysql/MysqlIndexMetadataParserTest.java
[OI]: hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/oracle/OracleIndexMetadataParserTest.java
[OR]: hsweb-easy-orm-rdb/src/test/java/org/hswebframework/ezorm/rdb/supports/oracle/OracleReactiveTests.java

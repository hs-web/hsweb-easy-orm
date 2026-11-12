# 一个简单的orm工具

[![Maven Central](https://img.shields.io/maven-central/v/org.hswebframework/hsweb-easy-orm.svg?style=plastic)](http://search.maven.org/#search%7Cga%7C1%7Chsweb-easy-orm)
![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/hs-web/hsweb-easy-orm/unit-test.yml?branch=master)
[![codecov](https://codecov.io/gh/hs-web/hsweb-easy-orm/branch/master/graph/badge.svg)](https://codecov.io/gh/hs-web/hsweb-easy-orm)


# 场景

查询可通过 `query.bindTables(new QueryTableBindings(tables))` 在服务端显式绑定本次操作的表映射，
JOIN 继续通过 `query.join(...)` 声明。FROM、JOIN、条件、投影和排序共用该映射，不替换共享 schema；
没有绑定时沿用原查找行为。绑定只保留在查询操作和其 clone 中，不进入 `QueryParam.context`，
普通 context 赋值不会安装映射或改变 JOIN。绑定不授权访问、不执行 DDL，调用方负责权限及映射生命周期。

1. 轻SQL,重java.
2. 动态表单: 动态维护表结构,增删改查.
3. 参数驱动动态条件, 前端也能透传动态条件,无SQL注入.
4. 通用条件可拓展, 不再局限`=,>,like...`. `where("userId","user-in-org",orgId)//查询指定机构下用户的数据`
5. 真响应式支持, 封装r2dbc. reactor真香.

# 🌰

```java

DatabaseOperator operator = ...;
//DDL
operator.ddl()
        .createOrAlter("test_table")
        .addColumn().name("id").number(32).primaryKey().comment("ID").commit()
        .addColumn().name("name").varchar(128).comment("名称").commit()
        .commit()
        .sync(); // reactive
     
//Query   
List<Map<String,Object>> dataList= operator.dml().query()
         .select("id")
         .from("test_table")
         .where(dsl->dsl.is("name","张三"))
         .fetch(mapList())
         .sync(); // reactive

```

# 使用

建议配合[hsweb4](https://github.com/hs-web/hsweb-framework/tree/4.0.x)使用.

## 版本分支

`4.2.3` 维护 `4.2.3-SNAPSHOT`，`4.2` 继续开发后续补丁版本。根 POM 和 core、RDB 的父版本需同步更新；模块间依赖使用 `${project.version}`。

自动发布工作流仅监听 `4.2` 和 `4.2.3` 的 push，使用分支 POM 中的版本部署到 Maven 仓库；Pull Request 沿用全量测试工作流。切分维护分支时，应先保留当前版本和对应发布触发，再升级开发分支版本。

## 实体字段类型

JPA 实体属性从属性描述符、getter 返回类型和 setter 参数类型解析 Java 类型；`Object` 属性保持 `Object`，支持只有 getter 或 setter 的属性。通过 `@ColumnType(type = ...)` 指定且实现 `ValueCodec` 的自定义数据类型可提供默认编解码器，显式 `@JsonCodec` 等编解码注解仍按现有规则优先生效。

`@ColumnType(exactString = true)` 显式要求字符串按大小写、重音及尾空格精确比较，方言负责列级 DDL：PostgreSQL 使用 `COLLATE "C"`，MySQL 8.0+ 使用 `utf8mb4_0900_bin`，H2 沿用其 VARCHAR 比较。其他方言若未实现则明确拒绝。默认 false 不改变已有列；它是建表 / 迁移元数据，不会在查询中插入 LOWER 或临时 CAST，也不会自动迁移已有列排序规则。

`@JsonCodec(includeNulls = true)` 为当前列保留显式嵌套 null；配置使用当前 codec 的独立 mapper，不修改共享默认配置。默认 false 保持原行为。JSON 编解码与物理列类型独立，可组合标准 LONGVARCHAR。
`@JsonCodec(useBigDecimalForFloats = true)` 将无具体 Java 数字类型的 JSON 小数解码为 BigDecimal，避免高精度正文读回时变成 Double；默认 false 保留原解码行为，可与 includeNulls 组合。

枚举位集合的 `in$any` 判断 `(M & Q) <> 0`，新增 `in$all` 判断 `(M & Q) = Q`（存储集合包含全部查询项），`nin` 使用对应否定。没有 any/all 的原 `in` 仍是 `(M & Q) = M`（存储集合是查询集合的子集）。通用 EnumInFragmentBuilder 同时接受枚举和已编码的 BIGINT mask，不要求业务选项映射到 Java ordinal。
`EnumInFragmentBuilder.of/ofNot` 统一选择数据库实现，包括 H2 的参数化 `BITAND`；调用方无需自行判断方言。

## 无有效更新列的保存

默认 UPDATE-then-INSERT 保存器对只有主键、不可更新列或省略值的输入，使用主键与原有 WHERE 条件查询存在性：匹配则作为已有记录完成保存，计入 `SaveResult.updated`，不发出物理 UPDATE；未匹配则沿用 INSERT 和唯一键冲突行为。Java null 仍表示省略，`NullValue` 仍表示显式清空。同步与响应式执行一致，不改变普通 UPDATE 的空列保护、原生数据库 upsert 或默认回退的并发保证。

## 已有条件分组

`Conditional.group()` 将已有条件整体分组，随后可继续追加条件。例如
`query.is("a", 1).or("b", 2).group().nest().is("c", 3).end()`
表示 `(a = 1 OR b = 2) AND (c = 3)`。空条件和单个条件不额外增加嵌套层级，
连续分组不会重复包裹；原 `nest()` / `orNest()` 仍只追加新分组。
`NestConditional.group()` 在当前嵌套层提供相同能力，例如
`query.nest().is("a", 1).or("b", 2).group().and("c", 3).end()`
表示 `((a = 1 OR b = 2) AND c = 3)`。仅重组当前层已有子条件，保留外层连接符、
承载条件的字段和值，以及 `end()` 返回的原父级构造器；支持多层嵌套。
内置 core、同步及响应式 Query/Update/Delete 均支持此能力，分组保留字段、分页及更新内容，
兼容只读条件列表。自定义构造器或代理需显式实现或委托 `group()`；默认实现抛出
`UnsupportedOperationException`，不会静默忽略分组。调用新方法的下游须同步使用支持该接口的 core/RDB 版本。

## 同步 CRUD 事件上下文

`hsweb-easy-orm-rdb` 的同步 Update/Delete DSL 携带 Repository 的 repository、columnMapping 等默认上下文。每次同步执行使用独立的 `EventContext`，before 写入的快照可在同次 after/error 中读取，SQL 结果或原异常也进入该上下文。错误通知再次失败时保留原异常，将通知异常附加为 suppressed。复用 DSL 或结果操作器再次执行时不会残留前次快照、结果或错误；未配置异常转换器时保留 JDBC 原异常。

实现入口为 `mapping/defaults/DefaultSyncRepository`、`DefaultSyncDelete`、`DefaultSyncUpdate`、`mapping/events/EventResultOperator` 和 `metadata/TableOrViewMetadata`。保留已发布的同步 DSL 构造签名及已有事件分发入口，新增重载；Reactive 事件链保持现有行为，不在上层树服务补发实体事件。

`SyncCrudEventContextIntegrationTest` 使用 H2 和正式 JDBC 执行器验证普通同步 Update/Delete 的 before/after/error、默认上下文、真实 SQL、监听器失败与调用方写入的事务回滚，以及重复执行隔离。`EventResultOperator` 还供 `DefaultRepository.doInsert` 和 `doSave` 使用，因此同一夹具直接验证单条/批量 insert/save 的真实 SQL、默认 keys 和 before/result/after 上下文。hsweb-framework 的严格树回归另行验收正式实体事件和监听器失败回滚，再重验下游资产维度完整链路。

集中验证入口：`mvn -pl hsweb-easy-orm-rdb -am -Dtest=SyncCrudEventContextIntegrationTest,DefaultSyncRepositoryTest,DefaultReactiveRepositoryTest,DefaultQueryUpdateCoverageTest,DefaultSyncQueryTest -Dsurefire.failIfNoSpecifiedTests=false test`。旧 CRUD 用例验证查询及操作兼容性，新用例验证 Update/Delete/insert/save 的正式事件代理路径；hsweb-framework 的 `SyncTreeCrudRegressionTest`、`ReactiveTreeCrudCoverageTest` 验证 insert/save/delete 的实体事件和事务链路。本修复基于 `4.2`，坐标为 `org.hswebframework:hsweb-easy-orm-rdb:4.2.2-SNAPSHOT`；交付时须先合入并发布可消费的 easy-orm 依赖，再发布 hsweb-framework 及下游 components。修复后的上游 snapshot 尚未正式发布，干净 CI 须先发布或从该上游源码正式构建依赖，不能依赖开发机的本地安装结果。

## 聚合计数

内置 `count` 函数默认生成 `count(列)`。查询方可通过 `SelectColumn.option("countRows", true)` 显式请求行数计数；表元数据确认目标列非空时生成 `count(*)`，此时 `countRows` 优先于 `distinct`、`arg` 等选项，其他情况沿用原有生成逻辑。调用方需确保关联查询不会将目标列扩展为 NULL。自定义 `count` 函数仍按其自身实现处理该选项；旧版本不识别 `countRows` 时继续按原有选项生成 SQL。实际性能取决于索引和执行计划。

## 响应式实体执行扩展

通过实体表元数据的 `addFeature(...)` 可按表注册以下可选 SPI，未注册的表保留已有 CRUD 行为及 before 事件触发时机：

- `ReactiveQueryResultTransformer` 在普通实体 `fetch` / `fetchOne` 的异步查询准备完成后变换结果流。每次订阅隔离查询参数和准备状态，`select_before` 监听器通过 `MappingContextKeys.reactiveQueryResultContext` 取得同次执行上下文，并可使用 `reactiveResultHolder.before(...)` 等待异步准备。该私有上下文的 `queryParam.sorts` 按原顺序包含参数排序和 DSL `orderBy(...)` 排序，保留函数、指定值及选项；准备阶段可统一校验、消费或改写，原查询不受影响。准备结果存入上下文的 `attributes`，供结果变换使用。`count` / `countLong` 参与准备但不变换聚合值；`fetchOne` 仍只消费一行且不应用列表分页偏移。
- `ReactiveCrudExecutionInterceptor` 在 INSERT/SAVE 的重复主键合并及 UPDATE/DELETE 的 SQL 构造前提供原始输入、修改值快照和执行条件。`proceed` 是保留正常 SQL 与生命周期事件的延迟调用，最多调用一次；拦截器可组合本地事务。UPDATE 的 `context.omitColumns(...)` 从物理更新中移除已消费的虚拟属性，不修改调用方实体；`context.executeVirtualUpdate(mutation)` 让仅虚拟字段的更新复用同一 prepare/before/after 事件链，等待 mutation 返回真实匹配行数，并避免主表 SQL。原生和虚拟 continuation 每次执行只能订阅其中一个一次，外层 UPDATE 重新订阅会创建新的执行上下文。启用拦截器的 UPDATE 事件通过 `MappingContextKeys.submittedValues` 保留移除虚拟字段前的只读提交值快照，`instance` 继续提供原实体引用。INSERT/UPDATE/DELETE 返回 `Integer`，SAVE 返回 `SaveResult`；集合输入是原始有序批次的只读快照，元素仍为调用方对象引用。

实现应组合并返回 Mono/Flux，保留订阅者 Reactor Context、权限条件、取消和错误，不自行 `subscribe()`。需要在同连接执行附加 SQL 的结果变换器可通过 `getBatchSize(context)` 返回正值，框架按段完整消费并释放当前 SQL Result 后再调用变换器，避免流式结果背压阻塞同事务后续查询。分段保留原排序并追加主键作为稳定顺序，保持调用方分页范围；每次订阅只准备一次，每段变换使用同一上下文。多段读取的可见性遵循当前事务隔离级别。默认返回 0 保持原单 SQL 和变换行为。聚合、自定义 SQL 和缺少实体标识的投影需使用独立的显式处理入口。CRUD SPI 不自动启动事务或补充权限，原始集合的批量边界也不代表上游 Publisher 自动流式分批；这些策略由注册方实现。

`save(Publisher)` 的输入流保留调用方 Context；Repository 的日志等 Context 扩展只应用于内部拦截器及原生持久化链，保留调用方已有的其他 key，拦截器也可在此基础上继续扩展。

接口位于 `hsweb-easy-orm-rdb` 的 `mapping` 包。`ReactiveQueryResultContext` 和 `ReactiveCrudExecutionContext` 均通过 `attributes` 保存单次执行状态；不要将请求状态保存在共享 Feature 实例中。

`ReactiveRepository.isResultCacheable()` 声明完整实体结果能否跨订阅进入仅按实体身份索引的共享缓存，默认 true。默认仓库委托已注册的 `ReactiveQueryResultTransformer.isResultCacheable()`，未注册时保留 true。转换器默认 false；仅当转换不依赖订阅者上下文，且其依赖数据更新能由既有缓存失效机制覆盖时，才可显式返回 true。缓存命中会跳过查询准备和转换；施加订阅者权限的仓库装饰器仍应返回 false，不能因目标仓库可缓存而放行。上层缓存服务负责缓存策略，不识别业务类型；该声明不控制数据库缓存或调用方显式划分范围的缓存。`GenericReactiveCacheSupportCrudServiceTest` 在下游 framework 验证纯转换的缓存命中、写后失效和上下文转换绕过缓存。

## MySQL 原生批量冲突更新

`operator.dml().upsert(table).nativeConflict(true)` 显式请求 MySQL 的多行 `INSERT ... ON DUPLICATE KEY UPDATE`。默认 `false` 保留额外唯一索引场景下的保守回退。MySQL 会响应任一唯一键冲突，因此仅在所有唯一键冲突都应更新同一业务记录时启用；此选项不提供 PostgreSQL 式的冲突目标选择。显式原生模式不支持 UPDATE WHERE 条件，存在条件时直接报错。更新列、主键保留及传入 null 时的已有处理语义保持不变。

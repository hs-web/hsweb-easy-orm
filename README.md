# 一个简单的orm工具

[![Maven Central](https://img.shields.io/maven-central/v/org.hswebframework/hsweb-easy-orm.svg?style=plastic)](http://search.maven.org/#search%7Cga%7C1%7Chsweb-easy-orm)
![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/hs-web/hsweb-easy-orm/unit-test.yml?branch=master)
[![codecov](https://codecov.io/gh/hs-web/hsweb-easy-orm/branch/master/graph/badge.svg)](https://codecov.io/gh/hs-web/hsweb-easy-orm)


# 场景

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

## 已有条件分组

`Conditional.group()` 将已有条件整体分组，随后可继续追加条件。例如
`query.is("a", 1).or("b", 2).group().nest().is("c", 3).end()`
表示 `(a = 1 OR b = 2) AND (c = 3)`。空条件和单个条件不额外增加嵌套层级，
连续分组不会重复包裹；原 `nest()` / `orNest()` 仍只追加新分组。
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

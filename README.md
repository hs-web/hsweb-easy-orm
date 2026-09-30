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

## 聚合计数

内置 `count` 函数默认生成 `count(列)`。查询方可通过 `SelectColumn.option("countRows", true)` 显式请求行数计数；表元数据确认目标列非空时生成 `count(*)`，此时 `countRows` 优先于 `distinct`、`arg` 等选项，其他情况沿用原有生成逻辑。调用方需确保关联查询不会将目标列扩展为 NULL。自定义 `count` 函数仍按其自身实现处理该选项；旧版本不识别 `countRows` 时继续按原有选项生成 SQL。实际性能取决于索引和执行计划。

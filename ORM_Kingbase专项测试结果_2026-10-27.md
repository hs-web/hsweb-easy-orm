# ORM Kingbase 专项测试结果

测试日期：2026-10-27。仅运行 ORM 的 Kingbase 相关单元测试、连接失败测试及 MySQL / Oracle 两种兼容模式的 R2DBC 实库测试，未运行原生数据库测试及 framework 测试。

## 测试结果

**BUILD SUCCESS：共 73 项，全部通过，失败 0、错误 0、跳过 0。** Maven 执行耗时约 1 分钟，完成时间为 16:08:34。

| 测试范围 | MySQL 模式 | Oracle 模式 | 合计 |
|---|---:|---:|---:|
| 单元测试：方言、Schema、元数据解析、异常转换 | 9 | 8 | 17 |
| 本地未监听端口的连接失败测试（不依赖数据库实例） | 1 | 1 | 2 |
| R2DBC 建立连接及兼容模式校验（实库） | 1 | 1 | 2 |
| ORM 综合集成测试（实库） | 26 | 26 | 52 |
| 合计 | 37 | 36 | 73 |

实库测试合计 54 项，两个模式各 27 项；其余 19 项不依赖数据库实例。已核对本次执行更新的 12 份 Surefire XML 报告，未混入历史测试结果。

实库覆盖连接与参数绑定、表和字段及索引元数据、数据类型回读、建表及结构变更、CRUD、批量写入、保存语义、实体仓储及关联、分页与查询条件、事务及保存点、重复键异常转换等现有测试场景。

## 驱动与连接

- 使用默认启用的 `kingbase-r2dbc` Profile。
- Maven 坐标：`com.github.zz97523:r2dbc-kingbase:main-SNAPSHOT`。
- 本次在线刷新 JitPack 元数据后解析版本：`main-be49b83f35-1`。
- MySQL 模式：`r2dbc:kingbase://127.0.0.1:54321/test?mode=mysql`。
- Oracle 模式：`r2dbc:kingbase://127.0.0.1:54322/test?mode=oracle`。
- 密码通过环境变量提供，不写入文档及源码。

## 执行命令

在 ORM 项目根目录执行；提前设置两个模式的连接密码环境变量 `KINGBASE_MYSQL_R2DBC_PASSWORD`、`KINGBASE_ORACLE_R2DBC_PASSWORD`：

```bash
mvn -B -pl hsweb-easy-orm-rdb \
  '-Dtest=Kingbase*Test,Kingbase*Tests' \
  -Dkingbase.mysql.r2dbc.integration=true \
  -Dkingbase.oracle.r2dbc.integration=true \
  -Duser.timezone=Asia/Shanghai \
  test
```

同时指定 `*Test` 和 `*Tests`，确保执行单元测试及以 `Tests` 结尾的实库测试。两个 integration 参数显式开启实库测试。

## 结果文件与范围说明

- 完整日志：`/tmp/hsweb-orm-kingbase-20261027-160625/test.log`。
- 本次 XML 报告快照：`/tmp/hsweb-orm-kingbase-20261027-160625/reports/`。
- 汇总数据：`/tmp/hsweb-orm-kingbase-20261027-160625/results.json`。
- Maven 原始报告目录：`hsweb-easy-orm-rdb/target/surefire-reports/`。

本次通过仅代表上述现有测试场景通过。此前确认的驱动时间参数毫秒丢失问题未修复，其严格复现测试位于 framework，不在本次 ORM 测试范围内。本轮未修改适配源码、测试代码或 POM。

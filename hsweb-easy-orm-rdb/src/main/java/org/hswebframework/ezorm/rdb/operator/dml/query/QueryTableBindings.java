package org.hswebframework.ezorm.rdb.operator.dml.query;

import java.util.Map;
import java.util.Optional;
import org.hswebframework.ezorm.rdb.metadata.TableOrViewMetadata;

/**
 * 本次查询显式使用的表映射。用于元数据快照、动态表等服务端已确认的映射，
 * 不修改共享 schema，也不安装 JOIN 或授予访问权限；调用方负责映射生命周期与授权。
 * 例如 {@code query.bindTables(new QueryTableBindings(tables)).join(join)}。
 *
 * @since 4.2.3
 */
public final class QueryTableBindings {
    private final Map<String, TableOrViewMetadata> tables;

    public QueryTableBindings(Map<String, TableOrViewMetadata> tables) {
        this.tables = Map.copyOf(tables);
    }

    /** 按查询引用名称解析显式绑定，适用于 FROM 和 JOIN；未绑定时由原 schema 解析。 */
    public Optional<TableOrViewMetadata> find(String name) { return Optional.ofNullable(tables.get(name)); }
}

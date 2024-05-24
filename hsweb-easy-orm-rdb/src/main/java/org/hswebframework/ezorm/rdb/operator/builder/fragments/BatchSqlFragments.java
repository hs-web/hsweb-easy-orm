package org.hswebframework.ezorm.rdb.operator.builder.fragments;

import org.apache.commons.collections4.CollectionUtils;
import org.hswebframework.ezorm.rdb.utils.FlatList;

import java.util.*;

public class BatchSqlFragments implements AppendableSqlFragments {

    private final List<List<String>> sql;
    private final List<List<Object>> parameter;

    public BatchSqlFragments() {
        this(32, 16);
    }

    public BatchSqlFragments(int sqlSize, int parameterSize) {
        sql = new ArrayList<>(sqlSize);
        parameter = new ArrayList<>(parameterSize);
    }

    @Override
    public boolean isEmpty() {
        return sql.isEmpty();
    }

    @Override
    public BatchSqlFragments addFragments(SqlFragments fragments) {
        return addSql(fragments.getSql())
            .addParameters(fragments.getParameters());
    }

    public BatchSqlFragments addSql(String sql) {
        return addSql(Collections.singletonList(sql));
    }

    public BatchSqlFragments addSql(Collection<String> sql) {
        List<String> _sql = sql instanceof List ? ((List<String>) sql) : new ArrayList<>(sql);
        return addSql(_sql);
    }

    public BatchSqlFragments addSql(List<String> sql) {
        if (!CollectionUtils.isEmpty(sql)) {
            this.sql.add(sql);
        }
        return this;
    }

    public BatchSqlFragments addSql(String... sql) {
        if (sql.length == 1) {
            return addSql(Collections.singletonList(sql[0]));
        }
        return addSql(Arrays.asList(sql));
    }

    public BatchSqlFragments addParameters(List<Object> parameters) {
        if (!CollectionUtils.isEmpty(parameters)) {
            this.parameter.add(parameters);
        }
        return this;
    }

    @Override
    @SuppressWarnings("all")
    public BatchSqlFragments addParameter(Collection<?> parameter) {
        return addParameters(parameter instanceof List ? ((List) parameter) : new ArrayList<>(parameter));
    }

    @Override
    public BatchSqlFragments addParameter(Object... parameter) {
        return addParameters(Arrays.asList(parameter));
    }

    @Override
    public List<String> getSql() {
        return new FlatList<>(sql);
    }

    @Override
    public List<Object> getParameters() {
        return new FlatList<>(parameter);
    }
}

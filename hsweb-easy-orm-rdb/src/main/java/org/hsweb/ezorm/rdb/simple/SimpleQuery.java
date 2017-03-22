package org.hsweb.ezorm.rdb.simple;

import org.hsweb.commons.StringUtils;
import org.hsweb.ezorm.core.*;
import org.hsweb.ezorm.core.param.QueryParam;
import org.hsweb.ezorm.core.param.SqlTerm;
import org.hsweb.ezorm.rdb.RDBQuery;
import org.hsweb.ezorm.rdb.executor.SQL;
import org.hsweb.ezorm.rdb.executor.SqlExecutor;
import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;
import org.hsweb.ezorm.rdb.render.SqlRender;
import org.hsweb.ezorm.rdb.simple.wrapper.TriggerWrapper;

import java.sql.SQLException;
import java.util.List;
import java.util.Map;

/**
 */
class SimpleQuery<T> extends ValidatorAndTriggerSupport<Query<T>> implements RDBQuery<T> {
    private SimpleTable<T> table;

    private QueryParam queryParam;

    private SqlRender<QueryParam> render;

    private SqlExecutor sqlExecutor;

    private SqlRender<QueryParam> totalRender;

    private ObjectWrapper objectWrapper;

    private Accepter accepter = this::and;

    public SimpleQuery(SimpleTable<T> table, SqlExecutor sqlExecutor, ObjectWrapper<T> objectWrapper) {
        this.table = table;
        this.render = table.getMeta().getDatabaseMetaData().getRenderer(SqlRender.TYPE.SELECT);
        this.totalRender = table.getMeta().getDatabaseMetaData().getRenderer(SqlRender.TYPE.SELECT_TOTAL);
        this.objectWrapper = new TriggerWrapper(table.getDatabase(), table, objectWrapper);
        this.sqlExecutor = sqlExecutor;
        this.queryParam = new QueryParam();
    }

    @Override
    public RDBQuery<T> setParam(QueryParam param) {
        this.queryParam = param;
        return this;
    }

    @Override
    public RDBQuery<T> select(String... fields) {
        this.queryParam.includes(fields);
        return this;
    }

    @Override
    public RDBQuery<T> selectExcludes(String... fields) {
        this.queryParam.excludes(fields);
        return this;
    }

    @Override
    protected Query<T> addSqlTerm(SqlTerm term) {
        queryParam.addTerm(term);
        return this;
    }

    @Override
    public RDBQuery<T> and() {
        setAnd();
        accepter = this::and;
        return this;
    }

    @Override
    public RDBQuery<T> or() {
        setOr();
        accepter = this::or;
        return this;
    }

    @Override
    public Accepter getAccepter() {
        return accepter;
    }

    @Override
    public RDBQuery<T> and(String condition, String termType, Object value) {
        queryParam.and(condition, termType, value);
        return this;
    }

    @Override
    public RDBQuery<T> or(String condition, String termType, Object value) {
        queryParam.or(condition, termType, value);
        return this;
    }

    @Override
    public NestConditional<Query<T>> nest() {
        return new SimpleNestConditional(this, this.queryParam.nest());
    }

    @Override
    public NestConditional<Query<T>> nest(String column, Object value) {
        return new SimpleNestConditional(this, this.queryParam.nest(column, value));
    }

    @Override
    public NestConditional<Query<T>> orNest() {
        return new SimpleNestConditional(this, this.queryParam.orNest());
    }

    @Override
    public NestConditional<Query<T>> orNest(String column, Object value) {
        return new SimpleNestConditional(this, this.queryParam.orNest(column, value));
    }

    @Override
    public RDBQuery<T> orderByAsc(String field) {
        this.queryParam.orderBy(field).asc();
        return this;
    }

    @Override
    public RDBQuery<T> orderByDesc(String field) {
        this.queryParam.orderBy(field).desc();
        return this;
    }

    @Override
    public RDBQuery<T> noPaging() {
        this.queryParam.setPaging(false);
        return this;
    }

    @Override
    public RDBQuery<T> forUpdate() {
        this.queryParam.setForUpdate(true);
        return this;
    }

    @Override
    public List<T> list() throws SQLException {
        QueryParam param = this.queryParam.clone();
        Map<String, Object> context = null;
        boolean supportBefore = !triggerSkip && getTableMeta().triggerIsSupport(Trigger.select_before);
        boolean supportDone = !triggerSkip && getTableMeta().triggerIsSupport(Trigger.select_before);
        if (supportBefore || supportDone) {
            context = table.getDatabase().getTriggerContextRoot();
            context.put("table", table);
            context.put("database", table.getDatabase());
            context.put("param", param);
            context.put("type", "single");
        }
        if (supportBefore) {
            trigger(Trigger.select_before, context);
        }
        param.setPaging(false);
        SQL sql = render.render(table.getMeta(), param);
        List<T> list = sqlExecutor.list(sql, objectWrapper);
        if (supportDone) {
            context.put("data", list);
            trigger(Trigger.select_done, context);
        }
        return list;
    }

    @Override
    public T single() throws SQLException {
        QueryParam param = this.queryParam.clone();
        Map<String, Object> context = null;
        boolean supportBefore = !triggerSkip && getTableMeta().triggerIsSupport(Trigger.select_before);
        boolean supportDone = !triggerSkip && getTableMeta().triggerIsSupport(Trigger.select_before);
        if (supportBefore || supportDone) {
            context = table.getDatabase().getTriggerContextRoot();
            context.put("table", table);
            context.put("database", table.getDatabase());
            context.put("param", param);
            context.put("type", "single");
        }
        if (supportBefore) {
            trigger(Trigger.select_before, context);
        }
        if (!param.isForUpdate())
            param.doPaging(0, 1);
        SQL sql = render.render(table.getMeta(), param);
        T data = (T) sqlExecutor.single(sql, objectWrapper);
        if (supportDone) {
            context.put("data", data);
            trigger(Trigger.select_done, context);
        }
        return data;
    }

    @Override
    public List<T> list(int pageIndex, int pageSize) throws SQLException {
        QueryParam param = queryParam.clone();
        Map<String, Object> context = null;
        boolean supportBefore = !triggerSkip && getTableMeta().triggerIsSupport(Trigger.select_before);
        boolean supportDone = !triggerSkip && getTableMeta().triggerIsSupport(Trigger.select_before);
        if (supportBefore || supportDone) {
            context = table.getDatabase().getTriggerContextRoot();
            context.put("table", table);
            context.put("database", table.getDatabase());
            context.put("param", param);
            context.put("type", "listPage");
        }
        if (supportBefore) {
            trigger(Trigger.select_before, context);
        }
        param.doPaging(pageIndex, pageSize);
        SQL sql = render.render(table.getMeta(), param.doPaging(pageIndex, pageSize));
        List<T> list = sqlExecutor.list(sql, objectWrapper);
        if (supportDone) {
            context.put("data", list);
            trigger(Trigger.select_done, context);
        }
        return list;
    }

    @Override
    public int total() throws SQLException {
        QueryParam param = this.queryParam.clone();
        Map<String, Object> context = null;
        boolean supportBefore = !triggerSkip && getTableMeta().triggerIsSupport(Trigger.select_before);
        boolean supportDone = !triggerSkip && getTableMeta().triggerIsSupport(Trigger.select_before);
        if (supportBefore || supportDone) {
            context = table.getDatabase().getTriggerContextRoot();
            context.put("table", table);
            context.put("database", table.getDatabase());
            context.put("param", param);
            context.put("type", "total");
        }
        if (supportBefore) {
            trigger(Trigger.select_before, context);
        }
        SQL sql = totalRender.render(table.getMeta(), param);
        TotalWrapper totalWrapper = new TotalWrapper();
        sqlExecutor.single(sql, totalWrapper);
        if (supportDone) {
            context.put("total", totalWrapper.getTotal());
            trigger(Trigger.select_done, context);
        }
        return totalWrapper.getTotal();
    }

    @Override
    RDBTableMetaData getTableMeta() {
        return table.getMeta();
    }

    public static class TotalWrapper implements ObjectWrapper {
        @Override
        public Class<Object> getType() {
            return Object.class;
        }

        private int total;

        public int getTotal() {
            return total;
        }

        @Override
        public Object newInstance() {
            return new Object();
        }

        @Override
        public void wrapper(Object instance, int index, String attr, Object value) {
            if ("TOTAL".equals(attr.toUpperCase()))
                this.total = StringUtils.toInt(value);
        }

        @Override
        public void done(Object instance) {

        }
    }
}

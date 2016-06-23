package org.hsweb.ezorm.run.simple;

import org.hsweb.commons.StringUtils;
import org.hsweb.ezorm.meta.TableMetaData;
import org.hsweb.ezorm.meta.expand.ObjectWrapper;
import org.hsweb.ezorm.executor.SQL;
import org.hsweb.ezorm.executor.SqlExecutor;
import org.hsweb.ezorm.meta.expand.Trigger;
import org.hsweb.ezorm.param.QueryParam;
import org.hsweb.ezorm.param.Term;
import org.hsweb.ezorm.render.SqlRender;
import org.hsweb.ezorm.run.Query;
import org.hsweb.ezorm.run.simple.wrapper.TriggerWrapper;

import java.sql.SQLException;
import java.util.List;
import java.util.Map;

/**
 * Created by zhouhao on 16-6-4.
 */
class SimpleQuery<T> extends ValidatorAndTriggerSupport<Query<T>> implements Query<T> {
    private SimpleTable<T> table;

    private QueryParam queryParam;

    private SqlRender<QueryParam> render;

    private SqlExecutor sqlExecutor;

    private SqlRender<QueryParam> totalRender;

    private ObjectWrapper objectWrapper;

    public SimpleQuery(SimpleTable<T> table, SqlExecutor sqlExecutor, ObjectWrapper<T> objectWrapper) {
        this.table = table;
        this.render = table.getMeta().getDatabaseMetaData().getRenderer(SqlRender.TYPE.SELECT);
        this.totalRender = table.getMeta().getDatabaseMetaData().getRenderer(SqlRender.TYPE.SELECT_TOTAL);
        this.objectWrapper = new TriggerWrapper(table.getDatabase(), table, objectWrapper);
        this.sqlExecutor = sqlExecutor;
        this.queryParam = new QueryParam();
    }

    @Override
    public Query<T> setParam(QueryParam param) {
        this.queryParam = param;
        return this;
    }

    @Override
    public Query<T> select(String... fields) {
        this.queryParam.select(fields);
        return this;
    }

    @Override
    public Query<T> selectExcludes(String... fields) {
        this.queryParam.excludes(fields);
        return this;
    }

    @Override
    public Query<T> where(String condition, Object value) {
        queryParam.where(condition, value);
        return this;
    }

    @Override
    public Query<T> and(String condition, Object value) {
        queryParam.and(condition, value);
        return this;
    }

    @Override
    public Query<T> or(String condition, Object value) {
        queryParam.or(condition, value);
        return this;
    }

    @Override
    public Term nest() {
        return queryParam.nest();
    }

    @Override
    public Term nest(String condition, Object value) {
        return queryParam.nest(condition, value);
    }

    @Override
    public Term orNest(String condition, Object value) {
        return orNest(condition, value);
    }

    @Override
    public Query<T> orderByAsc(String field) {
        this.queryParam.orderBy(field).asc();
        return this;
    }

    @Override
    public Query<T> orderByDesc(String field) {
        this.queryParam.orderBy(field).desc();
        return this;
    }

    @Override
    public Query<T> noPaging() {
        this.queryParam.setPaging(false);
        return this;
    }

    @Override
    public Query<T> forUpdate() {
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
        SQL sql = render.render(table.getMeta(), this.queryParam.clone().doPaging(pageIndex, pageSize));
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
    TableMetaData getTableMeta() {
        return table.getMeta();
    }

    public static class TotalWrapper implements ObjectWrapper {
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

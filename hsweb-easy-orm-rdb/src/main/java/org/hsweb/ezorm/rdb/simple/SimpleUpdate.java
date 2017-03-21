package org.hsweb.ezorm.rdb.simple;

import org.apache.commons.beanutils.BeanUtils;
import org.hsweb.ezorm.core.*;
import org.hsweb.ezorm.core.param.SqlTerm;
import org.hsweb.ezorm.core.param.UpdateParam;
import org.hsweb.ezorm.rdb.executor.SQL;
import org.hsweb.ezorm.rdb.executor.SqlExecutor;
import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;
import org.hsweb.ezorm.rdb.render.SqlRender;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by zhouhao on 16-6-5.
 */
class SimpleUpdate<T> extends ValidatorAndTriggerSupport<Update<T>> implements Update<T> {
    private static final Logger logger = LoggerFactory.getLogger(Update.class);
    private UpdateParam    updateParam;
    private SimpleTable<T> table;
    private SqlExecutor    sqlExecutor;

    public SimpleUpdate(SimpleTable<T> table, SqlExecutor sqlExecutor) {
        this.table = table;
        this.sqlExecutor = sqlExecutor;
        updateParam = new UpdateParam();
    }

    @Override
    public Update<T> set(T data) {
        updateParam.setData(data);
        return this;
    }

    @Override
    public Update<T> set(String property, Object value) {
        if (updateParam.getData() == null) updateParam.setData(new HashMap<>());
        if (updateParam.getData() instanceof Map) {
            ((Map) updateParam.getData()).put(property, value);
        } else {
            try {
                BeanUtils.setProperty(updateParam.getData(), property, value);
            } catch (Exception e) {
                logger.warn("property error", e);
            }
        }
        return this;
    }

    @Override
    protected Update<T> addSqlTerm(SqlTerm term) {
        updateParam.addTerm(term);
        return this;
    }

    @Override
    public Update<T> includes(String... fields) {
        updateParam.includes(fields);
        return this;
    }

    @Override
    public Update<T> excludes(String... fields) {
        updateParam.excludes(fields);
        return this;
    }

    @Override
    public Update<T> and(String condition, String termType, Object value) {
        updateParam.and(condition, termType, value);
        return this;
    }

    @Override
    public Update<T> or(String condition, String termType, Object value) {
        updateParam.or(condition, termType, value);
        return this;
    }

    private Accepter accepter=this::and;

    @Override
    public Update<T> and() {
        setAnd();
        accepter = this::and;
        return this;
    }

    @Override
    public Update<T> or() {
        setOr();
        accepter = this::or;
        return this;
    }

    @Override
    public Accepter getAccepter() {
        return accepter;
    }

    @Override
    public NestConditional<Update<T>> nest() {
        return new SimpleNestConditional<>(this, updateParam.nest());
    }

    @Override
    public NestConditional<Update<T>> nest(String column, Object value) {
        return new SimpleNestConditional<>(this, updateParam.nest(column, value));
    }

    @Override
    public NestConditional<Update<T>> orNest() {
        return new SimpleNestConditional<>(this, updateParam.orNest());
    }

    @Override
    public NestConditional<Update<T>> orNest(String column, Object value) {
        return new SimpleNestConditional<>(this, updateParam.orNest(column, value));
    }

    @Override
    public Update<T> setParam(UpdateParam param) {
        this.updateParam = param;
        return this;
    }

    @Override
    public int exec() throws SQLException {
        boolean supportBefore = !triggerSkip && table.getMeta().triggerIsSupport(Trigger.update_before);
        boolean supportDone = !triggerSkip && table.getMeta().triggerIsSupport(Trigger.update_done);
        Map<String, Object> context = table.getDatabase().getTriggerContextRoot();
        if (supportBefore || supportDone) {
            context = table.getDatabase().getTriggerContextRoot();
            context.put("table", table);
            context.put("database", table.getDatabase());
            context.put("param", updateParam);
        }
        if (supportBefore) {
            table.getMeta().on(Trigger.update_before, context);
        }
        SqlRender<UpdateParam> render = table.getMeta().getDatabaseMetaData().getRenderer(SqlRender.TYPE.UPDATE);
        SQL sql = render.render(table.getMeta(), updateParam);
        tryValidate(updateParam.getData(), Validator.Operation.UPDATE);
        int total = sqlExecutor.update(sql);
        if (supportDone) {
            context.put("total", total);
            table.getMeta().on(Trigger.update_done, context);
        }
        return total;
    }

    @Override
    RDBTableMetaData getTableMeta() {
        return table.getMeta();
    }
}

package og.hsweb.ezorm.run.simple;

import og.hsweb.ezorm.executor.SQL;
import og.hsweb.ezorm.executor.SqlExecutor;
import og.hsweb.ezorm.meta.TableMetaData;
import og.hsweb.ezorm.meta.expand.Trigger;
import og.hsweb.ezorm.param.UpdateParam;
import og.hsweb.ezorm.render.SqlRender;
import og.hsweb.ezorm.run.Update;
import org.apache.commons.beanutils.BeanUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.sql.SQLException;
import java.util.HashMap;
import java.util.Map;

/**
 * Created by zhouhao on 16-6-5.
 */
class SimpleUpdate<T> extends ValidatorAndTriggerSupport implements Update<T> {
    private static final Logger logger = LoggerFactory.getLogger(Update.class);
    private UpdateParam updateParam;
    private SimpleTable<T> table;
    private SqlExecutor sqlExecutor;

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
    public Update<T> where(String condition, Object value) {
        updateParam.where(condition, value);
        return this;
    }

    @Override
    public Update<T> setParam(UpdateParam<T> param) {
        this.updateParam = param;
        return this;
    }

    @Override
    public int exec() throws SQLException {
        boolean supportBefore = table.getMeta().triggerIsSupport(Trigger.update_before);
        boolean supportDone = table.getMeta().triggerIsSupport(Trigger.update_done);
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
        int total = sqlExecutor.update(sql);
        if (supportDone) {
            context.put("total", total);
            table.getMeta().on(Trigger.update_done, context);
        }
        return total;
    }

    @Override
    TableMetaData getTableMeta() {
        return table.getMeta();
    }
}

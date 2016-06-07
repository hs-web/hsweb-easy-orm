package org.hsweb.ezorm.run.simple;

import org.hsweb.ezorm.executor.SQL;
import org.hsweb.ezorm.executor.SqlExecutor;
import org.hsweb.ezorm.meta.TableMetaData;
import org.hsweb.ezorm.meta.expand.Trigger;
import org.hsweb.ezorm.param.SqlParam;
import org.hsweb.ezorm.render.SqlRender;
import org.hsweb.ezorm.run.Delete;
import org.hsweb.ezorm.run.Update;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.sql.SQLException;
import java.util.Map;

/**
 * Created by zhouhao on 16-6-5.
 */
class SimpleDelete extends ValidatorAndTriggerSupport<Delete> implements Delete {
    private SqlParam param;
    private SimpleTable table;
    private SqlExecutor sqlExecutor;
    private TableMetaData tableMetaData;

    public SimpleDelete(SimpleTable table, SqlExecutor sqlExecutor) {
        this.table = table;
        this.sqlExecutor = sqlExecutor;
        this.param = new SqlParam();
        this.tableMetaData = table.getMeta();
    }

    @Override
    public Delete where(String condition, Object value) {
        param.where(condition, value);
        return this;
    }

    @Override
    public Delete setParam(SqlParam param) {
        this.param = param;
        return this;
    }

    @Override
    public int exec() throws SQLException {
        Map<String, Object> context = null;
        boolean supportBefore = !triggerSkip && tableMetaData.triggerIsSupport(Trigger.delete_before);
        boolean supportDone = !triggerSkip && tableMetaData.triggerIsSupport(Trigger.delete_done);
        if (supportBefore || supportDone) {
            context = table.getDatabase().getTriggerContextRoot();
            context.put("table", table);
            context.put("database", table.getDatabase());
            context.put("param", param);
        }
        if (supportBefore) {
            tableMetaData.on(Trigger.delete_before, context);
        }
        SqlRender<SqlParam> render = tableMetaData.getDatabaseMetaData().getRenderer(SqlRender.TYPE.DELETE);
        SQL sql = render.render(table.getMeta(), param);
        int size = sqlExecutor.delete(sql);
        if (supportDone) {
            context.put("total", size);
            tableMetaData.on(Trigger.delete_done, context);
        }
        return size;
    }

    @Override
    TableMetaData getTableMeta() {
        return tableMetaData;
    }
}

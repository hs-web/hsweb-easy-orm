package org.hsweb.ezorm.run.simple.wrapper;

import org.hsweb.ezorm.meta.TableMetaData;
import org.hsweb.ezorm.meta.expand.ObjectWrapper;
import org.hsweb.ezorm.meta.expand.Trigger;
import org.hsweb.ezorm.run.Database;
import org.hsweb.ezorm.run.Table;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by zhouhao on 16-6-5.
 */
public class TriggerWrapper implements ObjectWrapper<Object> {

    private ObjectWrapper defaultWrapper;
    private Table table;
    private Database database;
    private TableMetaData metaData;

    public TriggerWrapper(Database database, Table table, ObjectWrapper defaultWrapper) {
        this.defaultWrapper = defaultWrapper;
        this.database = database;
        this.table = table;
        this.metaData = table.getMeta();
    }

    @Override
    public Object newInstance() {
        return defaultWrapper.newInstance();
    }

    @Override
    public void wrapper(Object instance, int index, String attr, Object value) {
        if (metaData.triggerIsSupport(Trigger.select_wrapper_each)) {
            Map<String, Object> context = new HashMap<>();
            context.put("table", table);
            context.put("database", database);
            context.put("index", index);
            context.put("property", attr);
            context.put("value", value);
            context.put("instance", instance);
            metaData.on(Trigger.select_wrapper_each, context);
        }
        this.defaultWrapper.wrapper(instance, index, attr, value);
    }

    @Override
    public void done(Object instance) {
        if (metaData.triggerIsSupport(Trigger.select_wrapper_done)) {
            Map<String, Object> context = new HashMap<>();
            context.put("table", table);
            context.put("database", database);
            context.put("instance", instance);
            metaData.on(Trigger.select_wrapper_done, context);
        }
        this.defaultWrapper.done(instance);
    }

}

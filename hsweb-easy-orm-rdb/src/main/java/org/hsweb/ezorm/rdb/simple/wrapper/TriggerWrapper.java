package org.hsweb.ezorm.rdb.simple.wrapper;

import org.hsweb.ezorm.core.ObjectWrapper;
import org.hsweb.ezorm.core.Trigger;
import org.hsweb.ezorm.rdb.meta.RDBTableMetaData;
import org.hsweb.ezorm.rdb.RDBDatabase;
import org.hsweb.ezorm.rdb.RDBTable;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by zhouhao on 16-6-5.
 */
public class TriggerWrapper implements ObjectWrapper<Object> {

    private ObjectWrapper    defaultWrapper;
    private RDBTable         table;
    private RDBDatabase      database;
    private RDBTableMetaData metaData;

    public TriggerWrapper(RDBDatabase database, RDBTable table, ObjectWrapper defaultWrapper) {
        this.defaultWrapper = defaultWrapper;
        this.database = database;
        this.table = table;
        this.metaData = table.getMeta();
    }

    @Override
    public <C> Class<C> getType() {
        return defaultWrapper.getType();
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

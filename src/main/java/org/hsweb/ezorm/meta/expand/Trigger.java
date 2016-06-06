package org.hsweb.ezorm.meta.expand;

import org.hsweb.ezorm.exception.TriggerException;

import java.util.Map;

public interface Trigger {

    void execute(Map<String, Object> context) throws TriggerException;

    String select_before = "select.before";
    String select_wrapper_each = "select.wrapper.each";
    String select_wrapper_done = "select.wrapper.done";
    String select_done = "select.done";
    String insert_before = "insert.before";
    String insert_done = "insert.done";
    String update_before = "update.before";
    String update_done = "update.done";
    String delete_before = "delete.before";
    String delete_done = "delete.done";
}

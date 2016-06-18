package org.hsweb.ezorm.run.simple.trigger;

import org.hsweb.commons.StringUtils;
import org.hsweb.expands.script.engine.DynamicScriptEngine;
import org.hsweb.expands.script.engine.ExecuteResult;
import org.hsweb.ezorm.exception.TriggerException;
import org.hsweb.ezorm.meta.expand.Trigger;

import java.util.Map;

/**
 * Created by zhouhao on 16-6-5.
 */
public class ScriptTraggerSupport implements Trigger {
    private String scriptId;

    private DynamicScriptEngine engine;

    public ScriptTraggerSupport(DynamicScriptEngine engine, String scriptId) {
        this.engine = engine;
        this.scriptId = scriptId;
    }

    @Override
    public void execute(Map<String, Object> context) throws TriggerException {
        boolean scriptCompiled = engine.compiled(scriptId);
        if (!scriptCompiled) {
            throw new TriggerException("动态脚本 [" + scriptId + "] 未编译!");
        }
        ExecuteResult result = engine.execute(scriptId, context);
        if (result.isSuccess()) {
            Object rsl = result.getResult();
            if (rsl instanceof Boolean) {
                if (!((Boolean) rsl)) {
                    throw new TriggerException("脚本返回结果:false");
                }
            }
            if (rsl instanceof Map) {
                Map map = ((Map) rsl);
                if (!StringUtils.isTrue(map.get("success"))) {
                    throw new TriggerException(String.valueOf(map.get("message")));
                }
            }
        } else {
            throw new TriggerException(result.getMessage(), result.getException());
        }
    }
}

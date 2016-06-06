package org.hsweb.ezorm.param;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by zhouhao on 16-6-4.
 */
public class InsertParamMapParam extends InsertParam<Map<String, Object>> {

    @Override
    public Map<String, Object> getData() {
        if (getData() == null) setData(new HashMap<>());
        return super.getData();
    }

    public InsertParamMapParam value(String property, Object value) {
        getData().put(property, value);
        return this;
    }

    public InsertParamMapParam values(Map<String, Object> values) {
        getData().putAll(values);
        return this;
    }
}

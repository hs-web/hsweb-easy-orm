/*
 * Copyright 2016 http://github.com/hs-web
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.hsweb.ezorm.rdb.meta.converter;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.serializer.SerializerFeature;
import org.hsweb.ezorm.core.ValueConverter;

public class JSONValueConverter implements ValueConverter {
    private Class type;

    private ValueConverter parent;

    public JSONValueConverter(Class type) {
        this(type, null);
    }

    public JSONValueConverter(Class type, ValueConverter parent) {
        this.type = type;
        this.parent = parent;
    }

    @Override
    public Object getData(Object value) {
        return JSON.toJSONString(parent == null ? value : parent.getValue(value), SerializerFeature.WriteClassName);
    }

    @Override
    public Object getValue(Object data) {
        if (parent != null) data = parent.getValue(data);
        if (data instanceof String) {
            String str = (String) data;
            if (str.startsWith("[")) {
                return JSON.parseArray(str, type);
            }
            return JSON.parseObject(str, type);
        }
        return data;
    }
}

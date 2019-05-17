package org.hswebframework.easyorm;

import com.alibaba.fastjson.JSONObject;
import org.hswebframework.ezorm.core.param.Term;

public interface ElasticSearchTermTypeMapper {

    JSONObject doMapping(Term term, Class<?> valueType);

    default boolean isNot() {
        return false;
    }

}

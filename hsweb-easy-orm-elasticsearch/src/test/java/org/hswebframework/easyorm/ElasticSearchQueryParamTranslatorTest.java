package org.hswebframework.easyorm;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.serializer.SerializerFeature;
import org.hswebframework.ezorm.core.dsl.Query;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.junit.Test;

public class ElasticSearchQueryParamTranslatorTest {


    @Test
    public void test() {
        JSONObject json = Query.empty(new QueryParam())
                .where()
                .is("ownerId", "public")
                .and()
                .in("status", "NO_MATCH", "SUCCESS")
                .nest()
                    .or()
                    .is("ownerId", "张")
                    .or()
                    .is("ownerId", "李")
                .end()
                .execute(ElasticSearchQueryParamTranslator::translate);

        System.out.println(JSON.toJSONString(json, SerializerFeature.PrettyFormat, SerializerFeature.WriteDateUseDateFormat));

    }

}
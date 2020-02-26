package org.hswebframework.easyorm.elasticsearch;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.alibaba.fastjson.serializer.SerializerFeature;
import org.elasticsearch.common.Strings;
import org.elasticsearch.index.query.QueryBuilder;
import org.hswebframework.ezorm.core.dsl.Query;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.junit.Assert;
import org.junit.Test;

public class ElasticSearchQueryParamTranslatorTest {

    /**
     * ownerId = public and status in NO_MATCH,SUCCESS and (ownerId = 张 or ownerId = 李)
     */
    @Test
    public void test1() {
        QueryBuilder builder = Query.of()
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

        JSONObject jsonObject = JSON.parseObject(Strings.toString(builder));

        System.out.println(JSON.toJSONString(jsonObject,
                SerializerFeature.PrettyFormat, SerializerFeature.WriteDateUseDateFormat));

        Assert.assertEquals(jsonObject.getJSONObject("bool").getJSONArray("must").size(), 3);
    }

    /**
     * (ownerId = 张 or ownerId = 李 or (id = 1 and age = 20)) and ownerId = public and status in NO_MATCH,SUCCESS and name != 老王
     */
    @Test
    public void test2() {
        QueryBuilder builder = Query.of()
                .where()
                .nest()
                .is("ownerId", "张")
                .or()
                .is("ownerId", "李")
                .or()
                .nest()
                .or()
                .is("id", 1)
                .and()
                .is("age", 20)
                .end()
                .end()
                .is("ownerId", "public")
                .and()
                .in("status", "NO_MATCH", "SUCCESS")
                .and()
                .not("name", "老王")
                .execute(ElasticSearchQueryParamTranslator::translate);

        JSONObject jsonObject = JSON.parseObject(Strings.toString(builder));

        System.out.println(JSON.toJSONString(jsonObject,
                SerializerFeature.PrettyFormat, SerializerFeature.WriteDateUseDateFormat));

        Assert.assertEquals(jsonObject.getJSONObject("bool").getJSONArray("must").size(), 4);


    }
}
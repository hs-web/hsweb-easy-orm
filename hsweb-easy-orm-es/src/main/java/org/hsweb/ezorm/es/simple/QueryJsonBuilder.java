package org.hsweb.ezorm.es.simple;

import com.alibaba.fastjson.JSONObject;
import org.hsweb.ezorm.core.param.QueryParam;
import org.hsweb.ezorm.core.param.Term;
import org.hsweb.ezorm.core.param.TermType;
import org.hsweb.ezorm.es.meta.ESTableMetaData;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author zhouhao
 */
public class QueryJsonBuilder {
    static final Map<String, String> termTypeMapper = new HashMap<>();

    static {
        termTypeMapper.put(TermType.eq, "term");
        termTypeMapper.put(TermType.like, "match");

    }

    public static void main(String[] args) {
        QueryParam queryParam = new QueryParam().where("title", "我").or("title2", "aaa");
        queryParam.nest().and("aa", "bb").or("aaa", "vvv");

        JSONObject jsonObject = new QueryJsonBuilder().build(null, queryParam.getTerms());
        System.out.println(JSONObject.toJSONString(jsonObject, true));
    }

    public JSONObject build(ESTableMetaData tableMetaData, List<Term> terms) {
        JSONObject jsonObject = new JSONObject();
        terms.stream()
                // .filter(term -> tableMetaData.findColumn(term.getColumn()) != null)
                .forEach(term -> {
                    JSONObject termObjN = build(term);
                    if (term.getTerms() != null) {
                        JSONObject nest = build(tableMetaData, term.getTerms());
                        if (!nest.isEmpty()) {
                            termObjN.put("bool", nest);
                        }
                    }
                    if (Term.Type.and == term.getType()) {
                        jsonObject.put("must", termObjN);
                    } else {
                        jsonObject.put("should", termObjN);
                    }
                });
        return jsonObject;
    }

    public JSONObject build(Term term) {
        JSONObject jsonObject = new JSONObject();
        JSONObject valueObj = new JSONObject();
        valueObj.put(term.getColumn(), term.getValue());
        String type = termTypeMapper.get(term.getTermType());
        jsonObject.put(type == null ? term.getTermType() : type, valueObj);
        return jsonObject;
    }
}

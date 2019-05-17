package org.hswebframework.easyorm;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import org.hswebframework.ezorm.core.param.QueryParam;
import org.hswebframework.ezorm.core.param.Term;
import org.hswebframework.ezorm.core.param.TermType;

import java.util.*;

public class ElasticSearchQueryParamTranslator {

    private QueryParam queryParam;

    private List<JSONObject> must = new LinkedList<>();
    private List<JSONObject> must_not = new LinkedList<>();
    private List<JSONObject> should = new LinkedList<>();

    public static Map<String, ElasticSearchTermTypeMapper> mappers = new HashMap<>();

    static {
        //term
        registerMapper(TermType.eq, (term, valueType) -> {
            JSONObject jsonObject = new JSONObject();

            jsonObject.put("term", Collections.singletonMap(term.getColumn().concat(".keyword"), term.getValue()));

            return jsonObject;
        });
        mappers.put("is", mappers.get(TermType.eq));

        //like
        registerMapper(TermType.like, (term, valueType) -> {
            JSONObject jsonObject = new JSONObject();

            jsonObject.put("match", Collections.singletonMap(term.getColumn(), term.getValue()));

            return jsonObject;
        });

        //in
        registerMapper(TermType.in, ((term, valueType) -> {
            JSONObject terms = new JSONObject();

            terms.put("terms", Collections.singletonMap(term.getColumn().concat(".keyword"), convertToList(term.getValue())));

            return terms;
        }));

        //range
        registerMapper(TermType.btw, ((term, valueType) -> {
            JSONObject terms = new JSONObject();
            List<Object> convertList = convertToList(term.getValue());

            Map<String, Object> range = new LinkedHashMap<>();
            range.put("from", convertList.get(0));

            if (convertList.size() > 1) {
                range.put("to", convertList.get(1));
            }

            terms.put("range", Collections.singletonMap(term.getColumn(), range));

            return terms;
        }));

        //gt
        registerMapper(TermType.gt, ((term, valueType) -> {
            JSONObject terms = new JSONObject();
            terms.put("range", Collections.singletonMap(term.getColumn(), Collections.singletonMap("gt", term.getValue())));
            return terms;
        }));

        //gte
        registerMapper(TermType.gte, ((term, valueType) -> {
            JSONObject terms = new JSONObject();
            terms.put("range", Collections.singletonMap(term.getColumn(), Collections.singletonMap("gte", term.getValue())));
            return terms;
        }));

        //lt
        registerMapper(TermType.lt, ((term, valueType) -> {
            JSONObject terms = new JSONObject();
            terms.put("range", Collections.singletonMap(term.getColumn(), Collections.singletonMap("lt", term.getValue())));
            return terms;
        }));

        //lte
        registerMapper(TermType.lte, ((term, valueType) -> {
            JSONObject terms = new JSONObject();
            terms.put("range", Collections.singletonMap(term.getColumn(), Collections.singletonMap("lte", term.getValue())));
            return terms;
        }));

        //original
        registerMapper("original", ((term, valueType) -> {
            Object value = term.getValue();
            if (value instanceof String) {
                return JSON.parseObject(((String) value));
            }
            if (value instanceof Map) {
                return new JSONObject(((Map) value));
            }

            throw new UnsupportedOperationException("unsupported original type:" + term.getValue().getClass());
        }));
    }

    public static void registerMapper(String type, ElasticSearchTermTypeMapper mapper) {
        mappers.put(type, mapper);
    }

    public void parse() {
        doParse(queryParam.getTerms(), must, must_not, should);
    }

    private ElasticSearchQueryParamTranslator(QueryParam queryParam) {
        this.queryParam = queryParam;
    }

    protected void doParse(List<Term> terms,
                           List<JSONObject> must,
                           List<JSONObject> must_not,
                           List<JSONObject> should) {
        // JSONObject query = new JSONObject();

        for (Term term : terms) {

            //nested
            if (term.getColumn() == null || term.getColumn().isEmpty()) {

                List<Term> nests = term.getTerms();
                if (nests == null || nests.isEmpty()) {
                    continue;
                }
                JSONObject bool = new JSONObject();
                List<JSONObject> nestMust = new ArrayList<>(), nestMustNot = new ArrayList<>(), nestShould = new ArrayList<>();

                doParse(nests, nestMust, nestMustNot, nestShould);

                if (!nestMust.isEmpty()) {
                    bool.put("must", nestMust);
                }
                if (!nestMustNot.isEmpty()) {
                    bool.put("must_not", nestMustNot);
                }
                if (!nestShould.isEmpty()) {
                    bool.put("should", nestShould);
                }
                if (term.getType() == Term.Type.or) {
                    should.add(new JSONObject(Collections.singletonMap("bool", bool)));
                } else {
                    must.add(new JSONObject(Collections.singletonMap("bool", bool)));
                }
                continue;
            }

            ElasticSearchTermTypeMapper mapper = mappers.get(term.getTermType());
            if (mapper == null) {
                throw new UnsupportedOperationException("unsupported term type:" + term.getTermType());
            }
            JSONObject result = mapper.doMapping(term, term.getValue().getClass());

            if (mapper.isNot()) {
                must_not.add(result);
            } else if (term.getType() == Term.Type.or) {
                should.add(result);
            } else {
                must.add(result);
            }

        }
    }


    public static JSONObject translate(QueryParam queryParam) {
        ElasticSearchQueryParamTranslator translator = new ElasticSearchQueryParamTranslator(queryParam);

        translator.parse();

        JSONObject payload = new JSONObject();
        JSONObject query = new JSONObject();
        JSONObject bool = new JSONObject();
        if (!translator.must.isEmpty()) {
            bool.put("must", translator.must);
        }
        if (!translator.must_not.isEmpty()) {
            bool.put("must_not", translator.must_not);
        }
        if (!translator.should.isEmpty()) {
            bool.put("should", translator.should);
        }

        query.put("bool", bool);
        payload.put("query", query);

        if (queryParam.isPaging()) {
            payload.put("from", queryParam.getPageSize() * queryParam.getPageIndex());
            payload.put("size", queryParam.getPageSize());
        }

        return payload;
    }

    static List<Object> convertToList(Object value) {
        if (value == null) return new ArrayList<>();
        if (value instanceof List) return (List) value;
        if (value instanceof Collection) return new ArrayList<>(((Collection) value));
        if (value instanceof String) {
            String[] arr = ((String) value).split("[,]");
            Object[] objArr = new Object[arr.length];
            for (int i = 0; i < arr.length; i++) {
                String str = arr[i];
                Object val = str;
                objArr[i] = val;
            }
            return new ArrayList<>(Arrays.asList(objArr));
        } else if (value.getClass().isArray()) {
            return new ArrayList<>(Arrays.asList(((Object[]) value)));
        } else {
            return new ArrayList<>(Collections.singletonList(value));
        }
    }
}

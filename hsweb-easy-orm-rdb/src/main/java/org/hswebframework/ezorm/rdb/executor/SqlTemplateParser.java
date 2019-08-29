package org.hswebframework.ezorm.rdb.executor;


import lombok.Getter;
import lombok.Setter;
import lombok.SneakyThrows;
import org.hswebframework.ezorm.core.ObjectPropertyOperator;
import org.hswebframework.ezorm.rdb.config.GlobalConfig;
import org.hswebframework.ezorm.rdb.utils.PropertiesUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

public class SqlTemplateParser {
    private static final char[] DEFAULT_PREPARE_START_SYMBOL = "#{".toCharArray();

    private static final char[] DEFAULT_PREPARE_END_SYMBOL = "}".toCharArray();

    @Getter
    @Setter
    private char[] prepareStartSymbol = DEFAULT_PREPARE_START_SYMBOL;

    @Getter
    @Setter
    private char[] prepareEndSymbol = DEFAULT_PREPARE_END_SYMBOL;

    @Getter
    @Setter
    private String template;

    @Getter
    @Setter
    private Object parameter;

    private char[] templateArray;

    private int pos;

    private char symbol;

    private void init() {
        templateArray = template.toCharArray();
        pos = 0;
    }

    private boolean isPrepare() {
        for (char c : prepareStartSymbol) {
            if (c == symbol) {
                return true;
            }
        }
        return false;
    }

    private boolean isPrepareEnd() {
        for (char c : prepareEndSymbol) {
            if (c == symbol) {
                return true;
            }
        }
        return false;
    }

    private boolean next() {
        symbol = templateArray[pos++];
        return pos < templateArray.length;
    }

    @SneakyThrows
    protected Object getProperty(String name) {
        return GlobalConfig.getPropertyOperator()
                .getProperty(parameter, name)
                .orElse(null);
    }

    public SqlRequest parse(Function<String, Object> propertyMapping) {
        init();
        boolean inPrepare = false;
        int len = 0;
        char[] newArr = new char[templateArray.length];
        char[] expression = new char[128];
        int expressionPos = 0;
        List<Object> parameters = new ArrayList<>(64);

        while (next()) {
            if (isPrepare()) {
                inPrepare = true;
            } else if (isPrepareEnd()) {
                inPrepare = false;
                parameters.add(propertyMapping.apply(new String(expression, 0, expressionPos)));
                expressionPos = 0;
                newArr[len++] = '?';
            } else if (inPrepare) {
                expression[expressionPos++] = symbol;
            } else {
                newArr[len++] = symbol;
            }
        }

        if (isPrepareEnd()) {
            newArr[len++] = '?';
            parameters.add(propertyMapping.apply(new String(expression, 0, expressionPos)));
        } else {
            newArr[len++] = symbol;
        }

        return SqlRequests.prepare(new String(newArr, 0, len), parameters.toArray());
    }

    public SqlRequest parse() {
        return parse(this::getProperty);
    }

    public static SqlRequest parse(String template, Object parameter) {
        SqlTemplateParser parser = new SqlTemplateParser();
        parser.parameter = parameter;
        parser.template = template;
        return parser.parse();
    }

    public static SqlRequest parse(String template, Function<String, Object> parameterGetter) {
        SqlTemplateParser parser = new SqlTemplateParser();
        parser.template = template;
        return parser.parse(parameterGetter);
    }

}

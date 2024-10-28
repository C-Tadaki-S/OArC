#include <inttypes.h>

#define SINGLE_BIAS 127
#define SINGLE_EXPOENT 8
#define SINGLE_BITS 32
#define SINGLE_MANTISSA 23
#define SINGLE_SIGN_MASK 0x80000000  
#define SINGLE_EXP_MASK 0x7F800000  
#define SINGLE_MANTISSA_MASK 0x007FFFFF 

typedef int32_t mint;
typedef uint32_t mfloat;

mfloat floatsisf (mint i);
mint fixsfsi (mfloat a);
mfloat negsf2 (mfloat a);
mfloat addsf3 (mfloat a, mfloat b);
mfloat subsf3 (mfloat a, mfloat b);

//Converte um inteiro para a representação ponto flutuante
mfloat floatsisf (mint i){

    // Retorna se é zero (representações iguais)
    if (i == 0) {
        return 0;
    }

    // Determinar o sinal
    int32_t sinal = (i < 0) ? 1 : 0;
    if (sinal) {
        i = -i; 
    }

    // Encontrar potência de 2 do número
    // Acha posição do primeiro bit "1" mais significativo
    int32_t expoente = 31;
    while ((i & (1 << expoente)) == 0) {
        expoente--; 
    }
    
    // Ajusta o expoente
    expoente += SINGLE_BIAS;

    // Calcular a fração 
    uint32_t uMant = i;
    uint32_t lMant = 0;
    for (int a = 0; a < expoente - SINGLE_BIAS; a++) {
        lMant |= (uMant & 1) << (SINGLE_BITS - 1); 
        lMant = lMant >> 1;
        uMant = uMant >> 1;
    };

    //Shift dos bits de expoente + sinal - 1
    lMant = lMant >> SINGLE_EXPOENT;

    mfloat resultado = (sinal << (SINGLE_BITS - 1)) | (expoente << SINGLE_MANTISSA) | lMant;

    return resultado; 
}

//Converte um ponto flutuante para a representação inteira
mint fixsfsi (mfloat a){
    uint32_t mantissa = (a & SINGLE_MANTISSA_MASK);
    int32_t expoente = (a & SINGLE_EXP_MASK) >> SINGLE_MANTISSA;
    uint32_t sinal = (a & SINGLE_SIGN_MASK) >> (SINGLE_BITS - 1);
    
    // Hidden bit
    if (expoente != 0)
        mantissa |= (1 << SINGLE_MANTISSA);

    if (mantissa == 0 && expoente == 0)
        return 0;

    // Ajuste de bias
    expoente -= SINGLE_BIAS;
    
    int32_t valor;
    
    // Shifts para multiplicação da mantissa
    if(expoente  < 0)
        valor = 0;
    else if (expoente <= SINGLE_MANTISSA)
        valor = mantissa >> (SINGLE_MANTISSA - expoente);
    else 
        valor = mantissa << expoente - SINGLE_MANTISSA;
    
    if (sinal)
        valor *= -1;
    return valor;
}

// Retorna o negado de a
mfloat negsf2 (mfloat a){    
    return a ^ (1 << (SINGLE_BITS - 1));
}

// Retorna a soma entre a e b
mfloat addsf3 (mfloat a, mfloat b){
    uint32_t mantissa_a = a & SINGLE_MANTISSA_MASK;
    uint32_t mantissa_b = b & SINGLE_MANTISSA_MASK;
    int32_t expoente_a =((a & SINGLE_EXP_MASK) >> SINGLE_MANTISSA) - SINGLE_BIAS;
    int32_t expoente_b =((b & SINGLE_EXP_MASK) >> SINGLE_MANTISSA) - SINGLE_BIAS;
    uint32_t sinal_a = (a & SINGLE_SIGN_MASK) >> (SINGLE_BITS - 1);
    uint32_t sinal_b = (b & SINGLE_SIGN_MASK) >> (SINGLE_BITS - 1);

    // Adiciona hidden bits
    if (expoente_a != 0)
        mantissa_a |= (1 << SINGLE_MANTISSA);

    if (expoente_b != 0)
        mantissa_b |= (1 << SINGLE_MANTISSA);

    // Calcula diferença entre expoentes
    int32_t exp_diff = expoente_a - expoente_b;
    int32_t expoente_result = 0;
    uint32_t mantissa_1 = mantissa_a;
    uint32_t mantissa_2 = mantissa_b;

    int32_t shift_exp = 0;

    //Se a diferença for maior que a mantissa, limitar o shift ao tamanho dela
    if (exp_diff <= -(SINGLE_MANTISSA + 1))
        shift_exp = -(SINGLE_MANTISSA + 1);
    else if (exp_diff > (SINGLE_MANTISSA + 1))
        shift_exp = (SINGLE_MANTISSA + 1);
    else 
        shift_exp = exp_diff;

    //Shiftar mantissa do número menor
    //Expoente preservado é o maior
    if (shift_exp >= 0){
        expoente_result = expoente_a + SINGLE_BIAS;
        mantissa_2 = mantissa_b >> shift_exp;
    } else {
        expoente_result = expoente_b + SINGLE_BIAS;
        mantissa_1 = mantissa_a >> -shift_exp;
    }

    // Operação real entre os módulos dos operandos
    // real_operation = 1 -> subtração
    uint32_t real_operation = sinal_a ^ sinal_b;

    //Soma/subtração das mantissas 
    uint32_t mantissa_result; 
    if (real_operation)
        if(sinal_a)
            mantissa_result = mantissa_2 - mantissa_1;
        else 
            mantissa_result = mantissa_1 - mantissa_2;   
    else
        mantissa_result = mantissa_1 + mantissa_2;
    
    //Carry no bit 25
    int32_t carry = (mantissa_result >> (SINGLE_MANTISSA + 1)) & 1; 

    //Complementa somente se for subtração e houve carry 
    if (carry & real_operation) 
        mantissa_result = (~mantissa_result + 1);
    
    //Para soma, normaliza mantissa em caso de carry
    if (!real_operation) {
        mantissa_result = mantissa_result >> carry;
        expoente_result += carry;
    }

    //Contagem dos leading zeros e normalização
    int32_t leading_zeros = 0;
    while ((mantissa_result & (1 << (SINGLE_MANTISSA - leading_zeros))) == 0 && mantissa_result != 0) {
        leading_zeros += 1; 
    }
    
    if (mantissa_result == 0)
        expoente_result = 0;
    else 
        expoente_result -= leading_zeros;

    mantissa_result = (mantissa_result << leading_zeros) & SINGLE_MANTISSA_MASK;
    
    // Define sinal final do resultado
    int32_t sinal_result;
    if (real_operation)
        if (carry)
            sinal_result = 1;
        else
            sinal_result = 0;
    else
        sinal_result = sinal_a;

    //Ajuste e retorno do resultado
    sinal_result = sinal_result << (SINGLE_MANTISSA + SINGLE_EXPOENT);
    expoente_result = expoente_result << SINGLE_MANTISSA;
    mfloat result = (sinal_result | expoente_result | mantissa_result);

    return result;
}

// Retorna a subtração entre a e b
mfloat subsf3 (mfloat a, mfloat b){
    uint32_t mantissa_a = a & SINGLE_MANTISSA_MASK;
    uint32_t mantissa_b = b & SINGLE_MANTISSA_MASK;
    int32_t expoente_a =((a & SINGLE_EXP_MASK) >> SINGLE_MANTISSA) - SINGLE_BIAS;
    int32_t expoente_b =((b & SINGLE_EXP_MASK) >> SINGLE_MANTISSA) - SINGLE_BIAS;
    uint32_t sinal_a = (a & SINGLE_SIGN_MASK) >> (SINGLE_BITS - 1);
    uint32_t sinal_b = (b & SINGLE_SIGN_MASK) >> (SINGLE_BITS - 1);

    // Adiciona hidden bits
    if (expoente_a != 0)
        mantissa_a |= (1 << SINGLE_MANTISSA);

    if (expoente_b != 0)
        mantissa_b |= (1 << SINGLE_MANTISSA);

    // Calcula diferença entre expoentes
    int32_t exp_diff = expoente_a - expoente_b;
    int32_t expoente_result = 0;
    uint32_t mantissa_1 = mantissa_a;
    uint32_t mantissa_2 = mantissa_b;

    int32_t shift_exp = 0;

    //Se a diferença for maior que a mantissa, limitar o shift ao tamanho dela
    if (exp_diff <= -(SINGLE_MANTISSA + 1))
        shift_exp = -(SINGLE_MANTISSA + 1);
    else if (exp_diff > (SINGLE_MANTISSA + 1))
        shift_exp = (SINGLE_MANTISSA + 1);
    else 
        shift_exp = exp_diff;

    //Shiftar mantissa do número menor
    //Expoente preservado é o maior
    if (shift_exp >= 0){
        expoente_result = expoente_a + SINGLE_BIAS;
        mantissa_2 = mantissa_b >> shift_exp;
    } else {
        expoente_result = expoente_b + SINGLE_BIAS;
        mantissa_1 = mantissa_a >> -shift_exp;
    }

    // Operação real entre os módulos dos operandos
    // real_operation = 1 -> subtração
    uint32_t real_operation = sinal_a ^ sinal_b ^ 1;

    //Soma/subtração das mantissas 
    uint32_t mantissa_result; 
    if (real_operation)
        if(sinal_a)
            mantissa_result = mantissa_2 - mantissa_1;
        else 
            mantissa_result = mantissa_1 - mantissa_2;   
    else
        mantissa_result = mantissa_1 + mantissa_2;
    
    //Carry no bit 25
    int32_t carry = (mantissa_result >> (SINGLE_MANTISSA + 1)) & 1; 

    //Complementa somente se for subtração e houve carry 
    if (carry & real_operation) 
        mantissa_result = (~mantissa_result + 1);
    
    //Para soma, normaliza mantissa em caso de carry
    if (!real_operation) {
        mantissa_result = mantissa_result >> carry;
        expoente_result += carry;
    }

    //Contagem dos leading zeros e normalização
    int32_t leading_zeros = 0;
    while ((mantissa_result & (1 << (SINGLE_MANTISSA - leading_zeros))) == 0 && mantissa_result != 0) {
        leading_zeros += 1; 
    }
    
    if (mantissa_result == 0)
        expoente_result = 0;
    else 
        expoente_result -= leading_zeros;

    mantissa_result = (mantissa_result << leading_zeros) & SINGLE_MANTISSA_MASK;
    
    // Define sinal final do resultado
    int32_t sinal_result;
    if (real_operation)
        if (carry)
            sinal_result = 1;
        else
            sinal_result = 0;
    else
        sinal_result = sinal_a;

    //Ajuste e retorno do resultado
    sinal_result = sinal_result << (SINGLE_MANTISSA + SINGLE_EXPOENT);
    expoente_result = expoente_result << SINGLE_MANTISSA;
    mfloat result = (sinal_result | expoente_result | mantissa_result);

    return result;
}
